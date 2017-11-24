module Grawlix.Main
  ( main
  ) where

import Grawlix.Config
import Grawlix.Database
import Grawlix.Options
import Grawlix.Server
import Grawlix.Sync

main :: IO ()
main = do
  options <- getOptions
  config <- getConfig options
  connection <- getConnection config
  runSync connection
  runServer connection
