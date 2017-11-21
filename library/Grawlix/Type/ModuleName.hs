{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.ModuleName
  ( ModuleName
  , toModuleName
  , fromModuleName
  ) where

import qualified Data.Aeson as Json
import qualified Data.Text as Text

newtype ModuleName = ModuleName [Text.Text]
  deriving (Eq, Ord, Show, Json.ToJSON)

toModuleName :: [Text.Text] -> ModuleName
toModuleName = ModuleName

fromModuleName :: ModuleName -> [Text.Text]
fromModuleName (ModuleName x) = x
