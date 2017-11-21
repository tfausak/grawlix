module Grawlix.Type.RepoKindId
  ( RepoKindId
  , toRepoKindId
  , fromRepoKindId
  ) where

import Grawlix.Type.Common

newtype RepoKindId =
  RepoKindId Int32
  deriving (Eq, Show)

toRepoKindId :: Int32 -> RepoKindId
toRepoKindId = RepoKindId

fromRepoKindId :: RepoKindId -> Int32
fromRepoKindId (RepoKindId x) = x
