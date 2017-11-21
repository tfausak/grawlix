module Grawlix.Type.Benchmark
  ( Benchmark(..)
  ) where

import Grawlix.Type.BenchmarkName
import Grawlix.Type.Condition
import Grawlix.Type.Dependencies

data Benchmark = Benchmark
  { benchmarkName :: BenchmarkName
  , benchmarkCondition :: Condition
  , benchmarkDependencies :: Dependencies
  } deriving (Eq, Ord, Show)
