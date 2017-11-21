{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.RepoId
  ( RepoId
  , toRepoId
  , fromRepoId
  ) where

import qualified Data.Int as Int

newtype RepoId = RepoId Int.Int32
  deriving (Eq, Show)

toRepoId :: Int.Int32 -> RepoId
toRepoId = RepoId

fromRepoId :: RepoId -> Int.Int32
fromRepoId (RepoId x) = x
