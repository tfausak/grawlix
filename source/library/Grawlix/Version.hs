module Grawlix.Version
  ( version
  , versionString
  )
where

import qualified Data.Version as Version
import qualified Paths_grawlix as Package

version :: Version.Version
version = Package.version

versionString :: String
versionString = Version.showVersion version
