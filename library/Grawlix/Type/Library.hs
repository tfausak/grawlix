module Grawlix.Type.Library
  ( Library(..)
  ) where

import Grawlix.Type.Condition
import Grawlix.Type.Dependencies
import Grawlix.Type.LibraryName
import Grawlix.Type.ModuleNames

data Library = Library
  { libraryName :: LibraryName
  , libraryCondition :: Condition
  , libraryModules :: ModuleNames
  , libraryDependencies :: Dependencies
  } deriving (Eq, Ord, Show)
