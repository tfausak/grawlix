module Grawlix.Type.Executable
  ( Executable(..)
  ) where

import Grawlix.Type.Condition
import Grawlix.Type.Dependencies
import Grawlix.Type.ExecutableName

data Executable = Executable
  { executableName :: ExecutableName
  , executableCondition :: Condition
  , executableDependencies :: Dependencies
  } deriving (Eq, Ord, Show)
