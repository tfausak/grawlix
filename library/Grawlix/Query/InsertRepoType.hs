{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertRepoType
  ( insertRepoType
  ) where

import Grawlix.Query.Common
import Grawlix.Type.RepoType

insertRepoType :: Query RepoType ()
insertRepoType =
  makeQuery
    [string|
      insert into repo_types ( content )
      values ( $1 )
      on conflict do nothing
    |]
    (contramap fromRepoType encodeText)
    decodeUnit
