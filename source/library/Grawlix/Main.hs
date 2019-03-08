module Grawlix.Main
  ( defaultMain
  )
where

import qualified Grawlix.Version as Version

defaultMain :: IO ()
defaultMain = putStrLn $ "grawlix-" <> Version.versionString
