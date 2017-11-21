{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Benchmarks
  ( Benchmarks
  , toBenchmarks
  , fromBenchmarks
  ) where

import Grawlix.Type.Benchmark
import Grawlix.Type.Common

newtype Benchmarks = Benchmarks (Set Benchmark)
  deriving (Eq, Show)

toBenchmarks :: Set Benchmark -> Benchmarks
toBenchmarks = Benchmarks

fromBenchmarks :: Benchmarks -> Set Benchmark
fromBenchmarks (Benchmarks x) = x
