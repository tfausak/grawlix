{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertBenchmark
  ( insertBenchmark
  ) where

import Grawlix.Query.Common
import Grawlix.Type.BenchmarkNameId
import Grawlix.Type.ConditionId
import Grawlix.Type.PackageId

insertBenchmark :: Query (PackageId, BenchmarkNameId, ConditionId) ()
insertBenchmark =
  makeQuery
    [string|
      insert into benchmarks ( package_id, benchmark_name_id, condition_id )
      values ( $1, $2, $3 )
      on conflict do nothing
    |]
    (contrazip3
       (contramap fromPackageId encodeInt32)
       (contramap fromBenchmarkNameId encodeInt32)
       (contramap fromConditionId encodeInt32))
    decodeUnit
