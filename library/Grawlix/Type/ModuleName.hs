{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.ModuleName
  ( ModuleName
  , toModuleName
  , fromModuleName
  ) where

import Grawlix.Type.Common

import qualified Data.Text as Text
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.Text as Cabal

newtype ModuleName =
  ModuleName [Text]
  deriving (Eq, Ord, Show, ToJSON)

instance FromHttpApiData ModuleName where
  parseUrlPiece =
    fmap (toModuleName . map Text.pack . Cabal.components) .
    maybe (fail "invalid module name") pure . Cabal.simpleParse . Text.unpack

toModuleName :: [Text] -> ModuleName
toModuleName = ModuleName

fromModuleName :: ModuleName -> [Text]
fromModuleName (ModuleName x) = x
