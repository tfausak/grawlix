{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.SelectExecutableId
  ( selectExecutableId
  ) where

import Grawlix.Query.Common
import Grawlix.Type.ConditionId
import Grawlix.Type.ExecutableId
import Grawlix.Type.ExecutableNameId
import Grawlix.Type.PackageId

import qualified Hasql.Decoders as D

selectExecutableId ::
     Query (PackageId, ExecutableNameId, ConditionId) ExecutableId
selectExecutableId =
  makeQuery
    [string|
      select id
      from executables
      where package_id = $1
      and executable_name_id = $2
      and condition_id = $3
    |]
    (contrazip3
       (contramap fromPackageId encodeInt32)
       (contramap fromExecutableNameId encodeInt32)
       (contramap fromConditionId encodeInt32))
    (toExecutableId <$> D.singleRow decodeInt32)
