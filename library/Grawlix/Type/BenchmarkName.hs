{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.BenchmarkName
  ( BenchmarkName
  , toBenchmarkName
  , fromBenchmarkName
  ) where

import qualified Data.Text as Text

newtype BenchmarkName = BenchmarkName Text.Text
  deriving (Eq, Ord, Show)

toBenchmarkName :: Text.Text -> BenchmarkName
toBenchmarkName = BenchmarkName

fromBenchmarkName :: BenchmarkName -> Text.Text
fromBenchmarkName (BenchmarkName x) = x
