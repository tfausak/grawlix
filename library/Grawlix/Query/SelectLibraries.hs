module Grawlix.Query.SelectLibraries
  ( selectLibraries
  ) where

import Grawlix.Query.Common
import Grawlix.Type.LibraryId
import Grawlix.Type.PackageName
import Grawlix.Type.Revision
import Grawlix.Type.Version

import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

selectLibraries :: Query (PackageName, Version, Revision) [LibraryId]
selectLibraries =
  makeQuery
    " select distinct libraries.id \
    \ from packages \
    \ inner join package_names \
    \ on package_names.id = packages.package_name_id \
    \ inner join versions \
    \ on versions.id = packages.version_id \
    \ inner join libraries \
    \ on libraries.package_id = packages.id \
    \ where package_names.content = $1 \
    \ and versions.content = $2 \
    \ and packages.revision = $3 \
    \ order by libraries.id asc "
    (contrazip3
       (contramap fromPackageName encodeText)
       (contramap fromVersion $ encodeList E.int4)
       (contramap fromRevision encodeInt32))
    (map toLibraryId <$> D.rowsList decodeInt32)
