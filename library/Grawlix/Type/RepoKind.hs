{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.RepoKind
  ( RepoKind
  , toRepoKind
  , fromRepoKind
  ) where

import Grawlix.Type.Common

newtype RepoKind = RepoKind Text
  deriving (Eq, Ord, Show)

toRepoKind :: Text -> RepoKind
toRepoKind = RepoKind

fromRepoKind :: RepoKind -> Text
fromRepoKind (RepoKind x) = x
