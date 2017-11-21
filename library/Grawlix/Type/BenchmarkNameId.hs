{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.BenchmarkNameId
  ( BenchmarkNameId
  , toBenchmarkNameId
  , fromBenchmarkNameId
  ) where

import Grawlix.Type.Common

newtype BenchmarkNameId = BenchmarkNameId Int32
  deriving (Eq, Show)

toBenchmarkNameId :: Int32 -> BenchmarkNameId
toBenchmarkNameId = BenchmarkNameId

fromBenchmarkNameId :: BenchmarkNameId -> Int32
fromBenchmarkNameId (BenchmarkNameId x) = x
