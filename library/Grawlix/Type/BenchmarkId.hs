{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.BenchmarkId
  ( BenchmarkId
  , toBenchmarkId
  , fromBenchmarkId
  ) where

import qualified Data.Int as Int

newtype BenchmarkId = BenchmarkId Int.Int32
  deriving (Eq, Show)

toBenchmarkId :: Int.Int32 -> BenchmarkId
toBenchmarkId = BenchmarkId

fromBenchmarkId :: BenchmarkId -> Int.Int32
fromBenchmarkId (BenchmarkId x) = x
