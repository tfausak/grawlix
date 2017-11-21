{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.BenchmarkNameId
  ( BenchmarkNameId
  , toBenchmarkNameId
  , fromBenchmarkNameId
  ) where

import qualified Data.Int as Int

newtype BenchmarkNameId = BenchmarkNameId Int.Int32
  deriving (Eq, Show)

toBenchmarkNameId :: Int.Int32 -> BenchmarkNameId
toBenchmarkNameId = BenchmarkNameId

fromBenchmarkNameId :: BenchmarkNameId -> Int.Int32
fromBenchmarkNameId (BenchmarkNameId x) = x
