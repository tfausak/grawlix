module Grawlix.Main
  ( main
  ) where

import Grawlix.Config
import Grawlix.Database
import Grawlix.Options
import Grawlix.Server
import Grawlix.Sync
import Grawlix.Type.Config

import qualified Control.Monad as Monad

main :: IO ()
main = do
  options <- getOptions
  config <- getConfig options
  connection <- getConnection config
  runMigrations config connection
  Monad.when (configSyncEnabled config) (runSync config connection)
  Monad.when (configServerEnabled config) (runServer connection)
