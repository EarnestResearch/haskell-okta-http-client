{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Okta.RIO.Client
  ( module Model
  , module Okta
  , callOkta
  , paginateOkta
  ) where

import qualified Data.ByteString.Char8 as C8
import           Data.Tuple
import qualified Network.HTTP.Client   as HTTP
import           Network.HTTP.Link     (LinkParam (..), href, linkParams,
                                        parseLinkHeaderBS)
import qualified Network.HTTP.Types    as HTTP
import           Network.URI           (uriQuery)
import           Okta
import           Okta.RIO.Model        as Model
import           Pipes                 as P
import           RIO


{- | Wraps generated client with runtime RIO environment.
     We try to extract Link if present.
     Mime decoding errors throw exceptions.
-}
callOkta
  :: ( Produces req accept
     , MimeUnrender accept res
     , MimeType contentType
     , HasOktaEnvironment env
     , HasLogFunc env
     )
  => OktaRequest req contentType res accept -- ^ request
  -> RIO env (res, Maybe NextToken) -- ^ response along with "next" token if any
callOkta oktaRequest = do
  connManager <- view getOktaHttpConnectionManager
  oktaConf <- view getOktaConfig
  MimeResult{..} <- liftIO $ dispatchMime connManager oktaConf oktaRequest
  logDebug . displayShow $ mimeResultResponse
  either (throwM . OktaException) (\r -> pure (r, nextTok mimeResultResponse)) mimeResult

  where
    -- Okta insists on "next" links, not tokens and only makes them available
    -- in the headers ... it's either this garbage or writing an alternative
    -- Okta client entry point that deals with raw URLs (as opposed to request objects)
    -- and parses results
    nextTok mr =
      let lnkHdrs = filter ((==) "Link" . fst) $ HTTP.responseHeaders mr
          lnks = concat $ catMaybes $ parseLinkHeaderBS . snd <$> lnkHdrs

          nxtTok = do nxtLnk <- listToMaybe $ filter (elem (Rel, "next") . linkParams) lnks
                      let query = (HTTP.parseQuery . C8.pack . uriQuery . href) nxtLnk
                      qAfter <- listToMaybe $ filter ((==) "after" . fst) query
                      snd qAfter

       in NextToken . decodeUtf8Lenient <$> nxtTok


{- | Runs paginated Okta requests. -}
paginateOkta
  :: ( Produces req accept
     , HasOptionalParam req After
     , MimeUnrender accept [res]
     , MimeType contentType
     , HasOktaEnvironment env
     , HasLogFunc env
     )
  => OktaRequest req contentType [res] accept -- ^ request
  -> Producer res (RIO env) () -- ^ producer of responses
paginateOkta req = runPaginateOkta id `P.for` P.each
  where
    runPaginateOkta reqMod = do
      (oktaRes, nextTok) <- lift $ callOkta (reqMod req)
      yield oktaRes
      case nextTok
        of Nothing -> pure ()
           Just (NextToken t) -> runPaginateOkta (`applyOptionalParam` After t)
