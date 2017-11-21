{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.ModuleNameId
  ( ModuleNameId
  , toModuleNameId
  , fromModuleNameId
  ) where

import Grawlix.Type.Common

newtype ModuleNameId = ModuleNameId Int32
  deriving (Eq, Show)

toModuleNameId :: Int32 -> ModuleNameId
toModuleNameId = ModuleNameId

fromModuleNameId :: ModuleNameId -> Int32
fromModuleNameId (ModuleNameId x) = x
