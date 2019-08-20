{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Lens.Micro.TH
import qualified Network.HTTP.Client as NH
import           Okta.RIO.Client
import           RIO
import           RIO.Process

-- | Command line arguments
data Options = Options
  { _optionsVerbose      :: !Bool
  , _optionsOktaHost     :: !Text
  , _optionsOktaApiToken :: !Text
  }

makeLenses ''Options

class HasOptions env where
  getOptions :: Lens' env Options


data App = App
  { _appLogFunc         :: !LogFunc
  , _appProcessContext  :: !ProcessContext
  , _appOptions         :: !Options
  , _appOktaConfig      :: !OktaConfig
  , _appHttpConnManager :: !NH.Manager
  }

makeLenses ''App

instance HasLogFunc App where
  logFuncL = appLogFunc

instance HasProcessContext App where
  processContextL = appProcessContext

instance HasOptions App where
  getOptions = appOptions

instance HasOktaEnvironment App where
  getOktaConfig = appOktaConfig
  getOktaHttpConnectionManager = appHttpConnManager
