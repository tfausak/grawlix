{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertLibraryModuleName
  ( insertLibraryModuleName
  ) where

import Grawlix.Query.Common
import Grawlix.Type.LibraryId
import Grawlix.Type.ModuleNameId

insertLibraryModuleName :: Query (LibraryId, ModuleNameId) ()
insertLibraryModuleName =
  makeQuery
    [string|
      insert into libraries_module_names ( library_id, module_name_id )
      values ( $1, $2 )
      on conflict do nothing
    |]
    (contrazip2
       (contramap fromLibraryId encodeInt32)
       (contramap fromModuleNameId encodeInt32))
    decodeUnit
