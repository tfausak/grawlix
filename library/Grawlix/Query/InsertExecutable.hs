{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertExecutable
  ( insertExecutable
  ) where

import Grawlix.Query.Common
import Grawlix.Type.ConditionId
import Grawlix.Type.ExecutableNameId
import Grawlix.Type.PackageId

import qualified Hasql.Decoders as D

insertExecutable :: Query (PackageId, ExecutableNameId, ConditionId) ()
insertExecutable =
  makeQuery
    [string|
      insert into executables ( package_id, executable_name_id, condition_id )
      values ( $1, $2, $3 )
      on conflict do nothing
    |]
    (contrazip3
       (contramap fromPackageId encodeInt32)
       (contramap fromExecutableNameId encodeInt32)
       (contramap fromConditionId encodeInt32))
    D.unit
