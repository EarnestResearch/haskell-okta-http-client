{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import           Import
import           Network.HTTP.Client.TLS
import qualified Okta                               as OktaClient
import           Options.Applicative.Simple
import qualified Paths_haskell_okta_http_client_rio
import qualified RIO.ByteString.Lazy                as LBS
import           RIO.Process
import           Run


main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_haskell_okta_http_client_rio.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
       <*> option str ( long "okta-host"
                     <> short 'H'
                     <> help "Okta host"
                     )
       <*> option str ( long "okta-api-token"
                     <> short 't'
                     <> help "Okta API token"
                     )
    )
    empty

  lo <- logOptionsHandle stderr (options ^. optionsVerbose)
  pc <- mkDefaultProcessContext
  mgr <- newTlsManager

  oktaConf0 <- liftIO $ OktaClient.withStderrLogging =<< OktaClient.newConfig
  let oktaConf = oktaConf0
        { OktaClient.configHost = LBS.fromStrict (encodeUtf8 ("https://" <> options ^. optionsOktaHost))
        } `OktaClient.addAuthMethod` OktaClient.AuthApiKeyApiToken ("SSWS " <> options ^. optionsOktaApiToken)

  withLogFunc lo $ \lf ->
    let app = App
          { _appLogFunc = lf
          , _appProcessContext = pc
          , _appOptions = options
          , _appOktaConfig = oktaConf
          , _appHttpConnManager = mgr
          }
     in runRIO app run
