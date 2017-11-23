{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.SelectModules
  ( selectModules
  ) where

import Grawlix.Query.Common
import Grawlix.Type.LibraryId
import Grawlix.Type.ModuleName
import Grawlix.Type.PackageName
import Grawlix.Type.Revision
import Grawlix.Type.Version

import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

selectModules :: Query (PackageName, Version, Revision, LibraryId) [ModuleName]
selectModules =
  makeQuery
    [string|
      select distinct module_names.content
      from packages
      inner join package_names
      on package_names.id = packages.package_name_id
      inner join versions
      on versions.id = packages.version_id
      inner join libraries
      on libraries.package_id = packages.id
      inner join libraries_module_names
      on libraries_module_names.library_id = libraries.id
      inner join module_names
      on module_names.id = libraries_module_names.module_name_id
      where package_names.content = $1
      and versions.content = $2
      and packages.revision = $3
      and libraries.id = $4
      order by module_names.content asc
    |]
    (contrazip4
       (contramap fromPackageName encodeText)
       (contramap fromVersion $ encodeList E.int4)
       (contramap fromRevision encodeInt32)
       (contramap fromLibraryId encodeInt32))
    (fmap (map toModuleName) . D.rowsList $ decodeList D.text)
