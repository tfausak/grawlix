module Grawlix.Query.SelectRepoTypeId
  ( selectRepoTypeId
  ) where

import Grawlix.Query.Common
import Grawlix.Type.RepoType
import Grawlix.Type.RepoTypeId

import qualified Hasql.Decoders as D

selectRepoTypeId :: Query RepoType RepoTypeId
selectRepoTypeId =
  makeQuery
    " select id \
    \ from repo_types \
    \ where content = $1 "
    (contramap fromRepoType encodeText)
    (toRepoTypeId <$> D.singleRow decodeInt32)
