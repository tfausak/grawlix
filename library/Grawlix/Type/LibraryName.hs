{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.LibraryName
  ( LibraryName
  , toLibraryName
  , fromLibraryName
  ) where

import qualified Data.Text as Text

newtype LibraryName = LibraryName Text.Text
  deriving (Eq, Ord, Show)

toLibraryName :: Text.Text -> LibraryName
toLibraryName = LibraryName

fromLibraryName :: LibraryName -> Text.Text
fromLibraryName (LibraryName x) = x
