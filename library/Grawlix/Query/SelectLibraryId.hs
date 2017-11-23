module Grawlix.Query.SelectLibraryId
  ( selectLibraryId
  ) where

import Grawlix.Query.Common
import Grawlix.Type.ConditionId
import Grawlix.Type.LibraryId
import Grawlix.Type.LibraryNameId
import Grawlix.Type.PackageId

import qualified Hasql.Decoders as D

selectLibraryId :: Query (PackageId, LibraryNameId, ConditionId) LibraryId
selectLibraryId =
  makeQuery
    " select id \
    \ from libraries \
    \ where package_id = $1 \
    \ and library_name_id = $2 \
    \ and condition_id = $3 "
    (contrazip3
       (contramap fromPackageId encodeInt32)
       (contramap fromLibraryNameId encodeInt32)
       (contramap fromConditionId encodeInt32))
    (toLibraryId <$> D.singleRow decodeInt32)
