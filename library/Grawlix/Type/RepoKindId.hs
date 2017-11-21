{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.RepoKindId
  ( RepoKindId
  , toRepoKindId
  , fromRepoKindId
  ) where

import qualified Data.Int as Int

newtype RepoKindId = RepoKindId Int.Int32
  deriving (Eq, Show)

toRepoKindId :: Int.Int32 -> RepoKindId
toRepoKindId = RepoKindId

fromRepoKindId :: RepoKindId -> Int.Int32
fromRepoKindId (RepoKindId x) = x
