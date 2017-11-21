module Grawlix.Type.RepoUrl
  ( RepoUrl
  , toRepoUrl
  , fromRepoUrl
  ) where

import Grawlix.Type.Common

newtype RepoUrl =
  RepoUrl Text
  deriving (Eq, Ord, Show)

toRepoUrl :: Text -> RepoUrl
toRepoUrl = RepoUrl

fromRepoUrl :: RepoUrl -> Text
fromRepoUrl (RepoUrl x) = x
