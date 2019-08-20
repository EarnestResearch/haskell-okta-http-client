{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import           Import
import           Okta.RIO.Client
import           Pipes           as P
import qualified Pipes.Prelude   as P

run :: RIO App ()
run = do
  logInfo "Printing list of all users"
  runEffect $ paginateOkta listUsers >-> P.print
