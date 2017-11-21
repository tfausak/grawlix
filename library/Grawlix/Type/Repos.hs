module Grawlix.Type.Repos
  ( Repos
  , toRepos
  , fromRepos
  ) where

import Grawlix.Type.Common
import Grawlix.Type.Repo

newtype Repos =
  Repos (Set Repo)
  deriving (Eq, Show)

toRepos :: Set Repo -> Repos
toRepos = Repos

fromRepos :: Repos -> Set Repo
fromRepos (Repos x) = x
