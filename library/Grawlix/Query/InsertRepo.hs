module Grawlix.Query.InsertRepo
  ( insertRepo
  ) where

import Grawlix.Query.Common
import Grawlix.Type.RepoKindId
import Grawlix.Type.RepoTypeId
import Grawlix.Type.RepoUrl

insertRepo :: Query (RepoKindId, RepoTypeId, RepoUrl) ()
insertRepo =
  makeQuery
    " insert into repos ( repo_kind_id, repo_type_id, url ) \
    \ values ( $1, $2, $3 ) \
    \ on conflict do nothing "
    (contrazip3
       (contramap fromRepoKindId encodeInt32)
       (contramap fromRepoTypeId encodeInt32)
       (contramap fromRepoUrl encodeText))
    decodeUnit
