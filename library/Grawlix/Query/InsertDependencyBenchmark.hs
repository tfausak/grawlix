{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertDependencyBenchmark
  ( insertDependencyBenchmark
  ) where

import Grawlix.Query.Common
import Grawlix.Type.BenchmarkId
import Grawlix.Type.DependencyId

insertDependencyBenchmark :: Query (DependencyId, BenchmarkId) ()
insertDependencyBenchmark =
  makeQuery
    [string|
      insert into dependencies_benchmarks ( dependency_id, benchmark_id )
      values ( $1, $2 )
      on conflict do nothing
    |]
    (contrazip2
       (contramap fromDependencyId encodeInt32)
       (contramap fromBenchmarkId encodeInt32))
    decodeUnit
