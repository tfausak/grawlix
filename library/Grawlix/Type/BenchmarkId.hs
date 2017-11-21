{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.BenchmarkId
  ( BenchmarkId
  , toBenchmarkId
  , fromBenchmarkId
  ) where

import Grawlix.Type.Common

newtype BenchmarkId = BenchmarkId Int32
  deriving (Eq, Show)

toBenchmarkId :: Int32 -> BenchmarkId
toBenchmarkId = BenchmarkId

fromBenchmarkId :: BenchmarkId -> Int32
fromBenchmarkId (BenchmarkId x) = x
