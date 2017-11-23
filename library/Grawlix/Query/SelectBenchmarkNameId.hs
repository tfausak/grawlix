{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.SelectBenchmarkNameId
  ( selectBenchmarkNameId
  ) where

import Grawlix.Query.Common
import Grawlix.Type.BenchmarkName
import Grawlix.Type.BenchmarkNameId

import qualified Hasql.Decoders as D

selectBenchmarkNameId :: Query BenchmarkName BenchmarkNameId
selectBenchmarkNameId =
  makeQuery
    [string|
      select id
      from benchmark_names
      where content = $1
    |]
    (contramap fromBenchmarkName encodeText)
    (toBenchmarkNameId <$> D.singleRow decodeInt32)
