{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertDependency
  ( insertDependency
  ) where

import Grawlix.Query.Common
import Grawlix.Type.ConstraintId
import Grawlix.Type.PackageNameId

insertDependency :: Query (ConstraintId, PackageNameId) ()
insertDependency =
  makeQuery
    [string|
      insert into dependencies ( constraint_id, package_name_id )
      values ( $1, $2 )
      on conflict do nothing
    |]
    (contrazip2
       (contramap fromConstraintId encodeInt32)
       (contramap fromPackageNameId encodeInt32))
    decodeUnit
