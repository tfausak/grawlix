{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.ModuleNames
  ( ModuleNames
  , toModuleNames
  , fromModuleNames
  ) where

import Grawlix.Type.Common
import Grawlix.Type.ModuleName

newtype ModuleNames = ModuleNames (Set ModuleName)
  deriving (Eq, Ord, Show)

toModuleNames :: Set ModuleName -> ModuleNames
toModuleNames = ModuleNames

fromModuleNames :: ModuleNames -> Set ModuleName
fromModuleNames (ModuleNames x) = x
