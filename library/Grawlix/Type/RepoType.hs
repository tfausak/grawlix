module Grawlix.Type.RepoType
  ( RepoType
  , toRepoType
  , fromRepoType
  ) where

import Grawlix.Type.Common

newtype RepoType =
  RepoType Text
  deriving (Eq, Ord, Show)

toRepoType :: Text -> RepoType
toRepoType = RepoType

fromRepoType :: RepoType -> Text
fromRepoType (RepoType x) = x
