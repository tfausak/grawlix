{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertDependencyLibrary
  ( insertDependencyLibrary
  ) where

import Grawlix.Query.Common
import Grawlix.Type.DependencyId
import Grawlix.Type.LibraryId

insertDependencyLibrary :: Query (DependencyId, LibraryId) ()
insertDependencyLibrary =
  makeQuery
    [string|
      insert into dependencies_libraries ( dependency_id, library_id )
      values ( $1, $2 )
      on conflict do nothing
    |]
    (contrazip2
       (contramap fromDependencyId encodeInt32)
       (contramap fromLibraryId encodeInt32))
    decodeUnit
