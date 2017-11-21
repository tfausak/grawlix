{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Repos
  ( Repos
  , toRepos
  , fromRepos
  ) where

import Grawlix.Type.Repo

import qualified Data.Set as Set

newtype Repos = Repos (Set.Set Repo)
  deriving (Eq, Show)

toRepos :: Set.Set Repo -> Repos
toRepos = Repos

fromRepos :: Repos -> Set.Set Repo
fromRepos (Repos x) = x
