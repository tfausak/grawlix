{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertTest
  ( insertTest
  ) where

import Grawlix.Query.Common
import Grawlix.Type.ConditionId
import Grawlix.Type.PackageId
import Grawlix.Type.TestNameId

insertTest :: Query (PackageId, TestNameId, ConditionId) ()
insertTest =
  makeQuery
    [string|
      insert into tests ( package_id, test_name_id, condition_id )
      values ( $1, $2, $3 )
      on conflict do nothing
    |]
    (contrazip3
       (contramap fromPackageId encodeInt32)
       (contramap fromTestNameId encodeInt32)
       (contramap fromConditionId encodeInt32))
    decodeUnit
