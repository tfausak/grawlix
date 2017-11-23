module Grawlix.Query.InsertPackageRepo
  ( insertPackageRepo
  ) where

import Grawlix.Query.Common
import Grawlix.Type.PackageId
import Grawlix.Type.RepoId

insertPackageRepo :: Query (PackageId, RepoId) ()
insertPackageRepo =
  makeQuery
    " insert into packages_repos ( package_id, repo_id ) \
    \ values ( $1, $2 ) \
    \ on conflict do nothing "
    (contrazip2
       (contramap fromPackageId encodeInt32)
       (contramap fromRepoId encodeInt32))
    decodeUnit
