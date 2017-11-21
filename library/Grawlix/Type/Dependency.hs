module Grawlix.Type.Dependency
  ( Dependency(..)
  ) where

import Grawlix.Type.PackageName
import Grawlix.Type.VersionBound

data Dependency = Dependency
  { dependencyPackage :: PackageName
  , dependencyVersionBound :: VersionBound
  } deriving (Eq, Show)
