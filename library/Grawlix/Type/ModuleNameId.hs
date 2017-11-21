{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.ModuleNameId
  ( ModuleNameId
  , toModuleNameId
  , fromModuleNameId
  ) where

import qualified Data.Int as Int

newtype ModuleNameId = ModuleNameId Int.Int32
  deriving (Eq, Show)

toModuleNameId :: Int.Int32 -> ModuleNameId
toModuleNameId = ModuleNameId

fromModuleNameId :: ModuleNameId -> Int.Int32
fromModuleNameId (ModuleNameId x) = x
