{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Okta.RIO.Model where

import           Lens.Micro.TH
import qualified Network.HTTP.Client as NH
import           Okta
import           RIO


-- | Okta host / API token
data OktaAPISecret = OktaAPISecret
  { _oapisHost  :: !Text
  , _oapisToken :: !Text
  } deriving (Eq, Generic)

makeLenses ''OktaAPISecret


newtype OktaException = OktaException MimeError deriving Show
instance Exception OktaException

newtype NextToken = NextToken Text deriving (Eq, Show)

class HasOktaEnvironment env where
  getOktaConfig :: Lens' env OktaConfig
  getOktaApiSecret :: Lens' env OktaAPISecret
  getOktaHttpConnectionManager :: Lens' env NH.Manager
