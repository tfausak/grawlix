{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.SelectVersions
  ( selectVersions
  ) where

import Grawlix.Query.Common
import Grawlix.Type.PackageName
import Grawlix.Type.Version

import qualified Hasql.Decoders as D

selectVersions :: Query PackageName [Version]
selectVersions =
  makeQuery
    [string|
      select distinct versions.content
      from versions
      inner join packages
      on packages.version_id = versions.id
      inner join package_names
      on package_names.id = packages.package_name_id
      where package_names.content = $1
      order by versions.content asc
    |]
    (contramap fromPackageName encodeText)
    (fmap (map toVersion) . D.rowsList $ decodeList D.int4)
