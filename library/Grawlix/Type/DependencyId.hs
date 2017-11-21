{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.DependencyId
  ( DependencyId
  , toDependencyId
  , fromDependencyId
  ) where

import qualified Data.Int as Int

newtype DependencyId = DependencyId Int.Int32
  deriving (Eq, Show)

toDependencyId :: Int.Int32 -> DependencyId
toDependencyId = DependencyId

fromDependencyId :: DependencyId -> Int.Int32
fromDependencyId (DependencyId x) = x
