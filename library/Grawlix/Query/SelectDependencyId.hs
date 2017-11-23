module Grawlix.Query.SelectDependencyId
  ( selectDependencyId
  ) where

import Grawlix.Query.Common
import Grawlix.Type.ConstraintId
import Grawlix.Type.DependencyId
import Grawlix.Type.PackageNameId

import qualified Hasql.Decoders as D

selectDependencyId :: Query (ConstraintId, PackageNameId) DependencyId
selectDependencyId =
  makeQuery
    " select id \
    \ from dependencies \
    \ where constraint_id = $1 \
    \ and package_name_id = $2 "
    (contrazip2
       (contramap fromConstraintId encodeInt32)
       (contramap fromPackageNameId encodeInt32))
    (toDependencyId <$> D.singleRow decodeInt32)
