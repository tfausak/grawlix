module Grawlix.Type.DependencyId
  ( DependencyId
  , toDependencyId
  , fromDependencyId
  ) where

import Grawlix.Type.Common

newtype DependencyId =
  DependencyId Int32
  deriving (Eq, Show)

toDependencyId :: Int32 -> DependencyId
toDependencyId = DependencyId

fromDependencyId :: DependencyId -> Int32
fromDependencyId (DependencyId x) = x
