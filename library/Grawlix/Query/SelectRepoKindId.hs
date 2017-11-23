module Grawlix.Query.SelectRepoKindId
  ( selectRepoKindId
  ) where

import Grawlix.Query.Common
import Grawlix.Type.RepoKind
import Grawlix.Type.RepoKindId

import qualified Hasql.Decoders as D

selectRepoKindId :: Query RepoKind RepoKindId
selectRepoKindId =
  makeQuery
    " select id \
    \ from repo_kinds \
    \ where content = $1 "
    (contramap fromRepoKind encodeText)
    (toRepoKindId <$> D.singleRow decodeInt32)
