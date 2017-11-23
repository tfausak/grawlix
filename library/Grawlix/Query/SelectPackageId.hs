{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.SelectPackageId
  ( selectPackageId
  ) where

import Grawlix.Query.Common
import Grawlix.Type.PackageId
import Grawlix.Type.PackageName
import Grawlix.Type.Revision
import Grawlix.Type.Version

import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

selectPackageId :: Query (PackageName, Version, Revision) PackageId
selectPackageId =
  makeQuery
    [string|
      select packages.id
      from packages
      inner join package_names
      on package_names.id = packages.package_name_id
      inner join versions
      on versions.id = packages.version_id
      where package_names.content = $1
      and versions.content = $2
      and packages.revision = $3
    |]
    (contrazip3
       (contramap fromPackageName encodeText)
       (contramap fromVersion $ encodeList E.int4)
       (contramap fromRevision encodeInt32))
    (toPackageId <$> D.singleRow decodeInt32)
