module Grawlix.Query.InsertPackage
  ( insertPackage
  ) where

import Grawlix.Query.Common
import Grawlix.Type.Description
import Grawlix.Type.License
import Grawlix.Type.PackageName
import Grawlix.Type.PackageUrl
import Grawlix.Type.Revision
import Grawlix.Type.Synopsis
import Grawlix.Type.Version

import qualified Hasql.Encoders as E

insertPackage ::
     Query ( PackageName
           , Version
           , Revision
           , License
           , Synopsis
           , Description
           , PackageUrl) ()
insertPackage =
  makeQuery
    " insert into packages ( \
    \   package_name_id, \
    \   version_id, \
    \   revision, \
    \   license_id, \
    \   synopsis, \
    \   description, \
    \   url \
    \ ) values ( \
    \   ( select id from package_names where content = $1 ), \
    \   ( select id from versions where content = $2 ), \
    \   $3, \
    \   ( select id from licenses where content = $4 ), \
    \   $5, \
    \   $6, \
    \   $7 \
    \ ) on conflict do nothing "
    (contrazip7
       (contramap fromPackageName encodeText)
       (contramap fromVersion $ encodeList E.int4)
       (contramap fromRevision encodeInt32)
       (contramap fromLicense encodeText)
       (contramap fromSynopsis encodeText)
       (contramap fromDescription encodeText)
       (contramap fromPackageUrl encodeText))
    decodeUnit
