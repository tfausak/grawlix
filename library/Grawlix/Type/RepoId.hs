module Grawlix.Type.RepoId
  ( RepoId
  , toRepoId
  , fromRepoId
  ) where

import Grawlix.Type.Common

newtype RepoId =
  RepoId Int32
  deriving (Eq, Show)

toRepoId :: Int32 -> RepoId
toRepoId = RepoId

fromRepoId :: RepoId -> Int32
fromRepoId (RepoId x) = x
