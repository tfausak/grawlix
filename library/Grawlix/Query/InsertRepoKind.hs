{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertRepoKind
  ( insertRepoKind
  ) where

import Grawlix.Query.Common
import Grawlix.Type.RepoKind

insertRepoKind :: Query RepoKind ()
insertRepoKind =
  makeQuery
    [string|
      insert into repo_kinds ( content )
      values ( $1 )
      on conflict do nothing
    |]
    (contramap fromRepoKind encodeText)
    decodeUnit
