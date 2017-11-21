{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Dependencies
  ( Dependencies
  , toDependencies
  , fromDependencies
  ) where

import Grawlix.Type.Constraint
import Grawlix.Type.PackageName

import qualified Data.Map as Map

newtype Dependencies = Dependencies (Map.Map PackageName Constraint)
  deriving (Eq, Ord, Show)

toDependencies :: Map.Map PackageName Constraint -> Dependencies
toDependencies = Dependencies

fromDependencies :: Dependencies -> Map.Map PackageName Constraint
fromDependencies (Dependencies x) = x
