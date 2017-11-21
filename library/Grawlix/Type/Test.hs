module Grawlix.Type.Test
  ( Test(..)
  ) where

import Grawlix.Type.Condition
import Grawlix.Type.Dependencies
import Grawlix.Type.TestName

data Test = Test
  { testName :: TestName
  , testCondition :: Condition
  , testDependencies :: Dependencies
  } deriving (Eq, Ord, Show)
