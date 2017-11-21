{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.RepoKind
  ( RepoKind
  , toRepoKind
  , fromRepoKind
  ) where

import qualified Data.Text as Text

newtype RepoKind = RepoKind Text.Text
  deriving (Eq, Ord, Show)

toRepoKind :: Text.Text -> RepoKind
toRepoKind = RepoKind

fromRepoKind :: RepoKind -> Text.Text
fromRepoKind (RepoKind x) = x
