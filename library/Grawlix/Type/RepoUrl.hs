{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.RepoUrl
  ( RepoUrl
  , toRepoUrl
  , fromRepoUrl
  ) where

import qualified Data.Text as Text

newtype RepoUrl = RepoUrl Text.Text
  deriving (Eq, Ord, Show)

toRepoUrl :: Text.Text -> RepoUrl
toRepoUrl = RepoUrl

fromRepoUrl :: RepoUrl -> Text.Text
fromRepoUrl (RepoUrl x) = x
