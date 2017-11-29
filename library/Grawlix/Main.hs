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
import qualified Data.Time as Time
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
           sleep $ configSyncDelay config
    else sleepForever

runServerForever :: Config -> Sql.Connection -> IO ()
runServerForever config connection =
  if configServerEnabled config
    then runServer connection
    else sleepForever

sleepForever :: IO ()
sleepForever = Monad.forever . sleep $ Time.secondsToDiffTime 1

sleep :: Time.DiffTime -> IO ()
sleep = Concurrent.threadDelay . picoToMicro . Time.diffTimeToPicoseconds

picoToMicro :: Integer -> Int
picoToMicro = integerToInt . (`div` 1000000)

integerToInt :: Integer -> Int
integerToInt x =
  let h = maxBound :: Int
      l = minBound :: Int
  in if x > fromIntegral h
       then h
       else if x < fromIntegral l
              then l
              else fromIntegral x
