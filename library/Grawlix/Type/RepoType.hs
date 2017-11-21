{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.RepoType
  ( RepoType
  , toRepoType
  , fromRepoType
  ) where

import qualified Data.Text as Text

newtype RepoType = RepoType Text.Text
  deriving (Eq, Ord, Show)

toRepoType :: Text.Text -> RepoType
toRepoType = RepoType

fromRepoType :: RepoType -> Text.Text
fromRepoType (RepoType x) = x
