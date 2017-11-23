{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.SelectPackageNameId
  ( selectPackageNameId
  ) where

import Grawlix.Query.Common
import Grawlix.Type.PackageName
import Grawlix.Type.PackageNameId

import qualified Hasql.Decoders as D

selectPackageNameId :: Query PackageName PackageNameId
selectPackageNameId =
  makeQuery
    [string|
      select id
      from package_names
      where content = $1
    |]
    (contramap fromPackageName encodeText)
    (toPackageNameId <$> D.singleRow decodeInt32)
