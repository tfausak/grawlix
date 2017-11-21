{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.ModuleName
  ( ModuleName
  , toModuleName
  , fromModuleName
  ) where

import Grawlix.Type.Common

newtype ModuleName =
  ModuleName [Text]
  deriving (Eq, Ord, Show, ToJSON)

toModuleName :: [Text] -> ModuleName
toModuleName = ModuleName

fromModuleName :: ModuleName -> [Text]
fromModuleName (ModuleName x) = x
