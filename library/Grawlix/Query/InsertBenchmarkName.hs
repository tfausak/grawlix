{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertBenchmarkName
  ( insertBenchmarkName
  ) where

import Grawlix.Query.Common
import Grawlix.Type.BenchmarkName

insertBenchmarkName :: Query BenchmarkName ()
insertBenchmarkName =
  makeQuery
    [string|
      insert into benchmark_names ( content )
      values ( $1 )
      on conflict do nothing
    |]
    (contramap fromBenchmarkName encodeText)
    decodeUnit
