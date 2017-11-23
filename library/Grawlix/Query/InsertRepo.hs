{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertRepo
  ( insertRepo
  ) where

import Grawlix.Query.Common
import Grawlix.Type.RepoKindId
import Grawlix.Type.RepoTypeId
import Grawlix.Type.RepoUrl

import qualified Hasql.Decoders as D

insertRepo :: Query (RepoKindId, RepoTypeId, RepoUrl) ()
insertRepo =
  makeQuery
    [string|
      insert into repos ( repo_kind_id, repo_type_id, url )
      values ( $1, $2, $3 )
      on conflict do nothing
    |]
    (contrazip3
       (contramap fromRepoKindId encodeInt32)
       (contramap fromRepoTypeId encodeInt32)
       (contramap fromRepoUrl encodeText))
    D.unit
