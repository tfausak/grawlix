{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.RepoTypeId
  ( RepoTypeId
  , toRepoTypeId
  , fromRepoTypeId
  ) where

import Grawlix.Type.Common

newtype RepoTypeId = RepoTypeId Int32
  deriving (Eq, Show)

toRepoTypeId :: Int32 -> RepoTypeId
toRepoTypeId = RepoTypeId

fromRepoTypeId :: RepoTypeId -> Int32
fromRepoTypeId (RepoTypeId x) = x
