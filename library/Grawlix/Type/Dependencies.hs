{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Dependencies
  ( Dependencies
  , toDependencies
  , fromDependencies
  ) where

import Grawlix.Type.Common
import Grawlix.Type.Constraint
import Grawlix.Type.PackageName

newtype Dependencies = Dependencies (Map PackageName Constraint)
  deriving (Eq, Ord, Show)

toDependencies :: Map PackageName Constraint -> Dependencies
toDependencies = Dependencies

fromDependencies :: Dependencies -> Map PackageName Constraint
fromDependencies (Dependencies x) = x
