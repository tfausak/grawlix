{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.ModuleNames
  ( ModuleNames
  , toModuleNames
  , fromModuleNames
  ) where

import Grawlix.Type.ModuleName

import qualified Data.Set as Set

newtype ModuleNames = ModuleNames (Set.Set ModuleName)
  deriving (Eq, Ord, Show)

toModuleNames :: Set.Set ModuleName -> ModuleNames
toModuleNames = ModuleNames

fromModuleNames :: ModuleNames -> Set.Set ModuleName
fromModuleNames (ModuleNames x) = x
