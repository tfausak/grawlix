module Grawlix.Type.Dependency
  ( Dependency(..)
  ) where

import Grawlix.Type.PackageName
import Grawlix.Type.VersionRange

data Dependency = Dependency
  { dependencyPackage :: PackageName
  , dependencyVersionRange :: VersionRange
  } deriving (Eq, Show)
