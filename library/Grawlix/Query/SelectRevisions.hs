module Grawlix.Query.SelectRevisions
  ( selectRevisions
  ) where

import Grawlix.Query.Common
import Grawlix.Type.PackageName
import Grawlix.Type.Revision
import Grawlix.Type.Version

import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

selectRevisions :: Query (PackageName, Version) [Revision]
selectRevisions =
  makeQuery
    " select distinct packages.revision \
    \ from packages \
    \ inner join package_names \
    \ on package_names.id = packages.package_name_id \
    \ inner join versions \
    \ on versions.id = packages.version_id \
    \ where package_names.content = $1 \
    \ and versions.content = $2 \
    \ order by packages.revision asc "
    (contrazip2
       (contramap fromPackageName encodeText)
       (contramap fromVersion $ encodeList E.int4))
    (map toRevision <$> D.rowsList decodeInt32)
