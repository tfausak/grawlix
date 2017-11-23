{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.SelectRepoId
  ( selectRepoId
  ) where

import Grawlix.Query.Common
import Grawlix.Type.RepoId
import Grawlix.Type.RepoKindId
import Grawlix.Type.RepoTypeId
import Grawlix.Type.RepoUrl

import qualified Hasql.Decoders as D

selectRepoId :: Query (RepoKindId, RepoTypeId, RepoUrl) RepoId
selectRepoId =
  makeQuery
    [string|
      select id
      from repos
      where repo_kind_id = $1
      and repo_type_id = $2
      and url = $3
    |]
    (contrazip3
       (contramap fromRepoKindId encodeInt32)
       (contramap fromRepoTypeId encodeInt32)
       (contramap fromRepoUrl encodeText))
    (toRepoId <$> D.singleRow decodeInt32)
