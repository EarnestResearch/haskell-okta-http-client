{-# LANGUAGE NoImplicitPrelude #-}

module Okta.RIO.Model where

import qualified Network.HTTP.Client as NH
import           Okta
import           RIO

newtype OktaException = OktaException MimeError deriving Show
instance Exception OktaException

newtype NextToken = NextToken Text deriving (Eq, Show)

class HasLogFunc env => HasOktaEnvironment env where
  getOktaConfig :: Lens' env OktaConfig
  getOktaHttpConnectionManager :: Lens' env NH.Manager
