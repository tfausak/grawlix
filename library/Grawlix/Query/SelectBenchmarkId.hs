{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.SelectBenchmarkId
  ( selectBenchmarkId
  ) where

import Grawlix.Query.Common
import Grawlix.Type.BenchmarkId
import Grawlix.Type.BenchmarkNameId
import Grawlix.Type.ConditionId
import Grawlix.Type.PackageId

import qualified Hasql.Decoders as D

selectBenchmarkId :: Query (PackageId, BenchmarkNameId, ConditionId) BenchmarkId
selectBenchmarkId =
  makeQuery
    [string|
      select id
      from benchmarks
      where package_id = $1
      and benchmark_name_id = $2
      and condition_id = $3
    |]
    (contrazip3
       (contramap fromPackageId encodeInt32)
       (contramap fromBenchmarkNameId encodeInt32)
       (contramap fromConditionId encodeInt32))
    (toBenchmarkId <$> D.singleRow decodeInt32)
