{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Benchmarks
  ( Benchmarks
  , toBenchmarks
  , fromBenchmarks
  ) where

import Grawlix.Type.Benchmark

import qualified Data.Set as Set

newtype Benchmarks = Benchmarks (Set.Set Benchmark)
  deriving (Eq, Show)

toBenchmarks :: Set.Set Benchmark -> Benchmarks
toBenchmarks = Benchmarks

fromBenchmarks :: Benchmarks -> Set.Set Benchmark
fromBenchmarks (Benchmarks x) = x
