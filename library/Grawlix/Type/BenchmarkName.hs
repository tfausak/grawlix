{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.BenchmarkName
  ( BenchmarkName
  , toBenchmarkName
  , fromBenchmarkName
  ) where

import Grawlix.Type.Common

newtype BenchmarkName = BenchmarkName Text
  deriving (Eq, Ord, Show)

toBenchmarkName :: Text -> BenchmarkName
toBenchmarkName = BenchmarkName

fromBenchmarkName :: BenchmarkName -> Text
fromBenchmarkName (BenchmarkName x) = x
