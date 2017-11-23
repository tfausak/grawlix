{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.SelectPackageNames
  ( selectPackageNames
  ) where

import Grawlix.Query.Common
import Grawlix.Type.PackageName

import qualified Hasql.Decoders as D

selectPackageNames :: Query () [PackageName]
selectPackageNames =
  makeQuery
    [string|
      select distinct content
      from package_names
      order by content asc
    |]
    encodeUnit
    (map toPackageName <$> D.rowsList decodeText)
