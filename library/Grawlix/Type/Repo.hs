module Grawlix.Type.Repo
  ( Repo(..)
  ) where

import Grawlix.Type.RepoKind
import Grawlix.Type.RepoType
import Grawlix.Type.RepoUrl

data Repo = Repo
  { repoKind :: RepoKind
  , repoType :: RepoType
  , repoUrl :: RepoUrl
  } deriving (Eq, Ord, Show)
