{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertCategoryPackage
  ( insertCategoryPackage
  ) where

import Grawlix.Query.Common
import Grawlix.Type.CategoryId
import Grawlix.Type.PackageId

import qualified Hasql.Decoders as D

insertCategoryPackage :: Query (CategoryId, PackageId) ()
insertCategoryPackage =
  makeQuery
    [string|
      insert into categories_packages ( category_id, package_id )
      values ( $1, $2 )
      on conflict do nothing
    |]
    (contrazip2
       (contramap fromCategoryId encodeInt32)
       (contramap fromPackageId encodeInt32))
    D.unit
