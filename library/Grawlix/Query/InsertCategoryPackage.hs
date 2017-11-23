module Grawlix.Query.InsertCategoryPackage
  ( insertCategoryPackage
  ) where

import Grawlix.Query.Common
import Grawlix.Type.CategoryId
import Grawlix.Type.PackageId

insertCategoryPackage :: Query (CategoryId, PackageId) ()
insertCategoryPackage =
  makeQuery
    " insert into categories_packages ( category_id, package_id ) \
    \ values ( $1, $2 ) \
    \ on conflict do nothing "
    (contrazip2
       (contramap fromCategoryId encodeInt32)
       (contramap fromPackageId encodeInt32))
    decodeUnit
