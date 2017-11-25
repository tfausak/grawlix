module Grawlix.Main
  ( main
  ) where

import Grawlix.Config
import Grawlix.Database
import Grawlix.Options
import Grawlix.Server
import Grawlix.Sync
import Grawlix.Type.Config

import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.Async as Async
import qualified Control.Monad as Monad
import qualified Hasql.Connection as Sql

main :: IO ()
main = do
  options <- getOptions
  config <- getConfig options
  connection <- getConnection config
  runMigrations config connection
  Async.race_
    (runSyncForever config connection)
    (runServerForever config connection)

runSyncForever :: Config -> Sql.Connection -> IO ()
runSyncForever config connection =
  if configSyncEnabled config
    then Monad.forever $ do
           runSync config connection
           Concurrent.threadDelay 60000000 -- 1 minute
    else sleepForever

runServerForever :: Config -> Sql.Connection -> IO ()
runServerForever config connection =
  if configServerEnabled config
    then runServer connection
    else sleepForever

sleepForever :: IO ()
sleepForever = Monad.forever $ Concurrent.threadDelay 1000000 -- 1 second
