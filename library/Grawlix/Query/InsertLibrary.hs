{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertLibrary
  ( insertLibrary
  ) where

import Grawlix.Query.Common
import Grawlix.Type.ConditionId
import Grawlix.Type.LibraryNameId
import Grawlix.Type.PackageId

insertLibrary :: Query (PackageId, LibraryNameId, ConditionId) ()
insertLibrary =
  makeQuery
    [string|
      insert into libraries ( package_id, library_name_id, condition_id )
      values ( $1, $2, $3 )
      on conflict do nothing
    |]
    (contrazip3
       (contramap fromPackageId encodeInt32)
       (contramap fromLibraryNameId encodeInt32)
       (contramap fromConditionId encodeInt32))
    decodeUnit
