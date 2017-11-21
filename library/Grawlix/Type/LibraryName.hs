{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.LibraryName
  ( LibraryName
  , toLibraryName
  , fromLibraryName
  ) where

import Grawlix.Type.Common

newtype LibraryName = LibraryName Text
  deriving (Eq, Ord, Show)

toLibraryName :: Text -> LibraryName
toLibraryName = LibraryName

fromLibraryName :: LibraryName -> Text
fromLibraryName (LibraryName x) = x
