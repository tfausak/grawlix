{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.RepoTypeId
  ( RepoTypeId
  , toRepoTypeId
  , fromRepoTypeId
  ) where

import qualified Data.Int as Int

newtype RepoTypeId = RepoTypeId Int.Int32
  deriving (Eq, Show)

toRepoTypeId :: Int.Int32 -> RepoTypeId
toRepoTypeId = RepoTypeId

fromRepoTypeId :: RepoTypeId -> Int.Int32
fromRepoTypeId (RepoTypeId x) = x
