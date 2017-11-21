{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.LibraryNameId
  ( LibraryNameId
  , toLibraryNameId
  , fromLibraryNameId
  ) where

import Grawlix.Type.Common

newtype LibraryNameId = LibraryNameId Int32
  deriving (Eq, Show)

toLibraryNameId :: Int32 -> LibraryNameId
toLibraryNameId = LibraryNameId

fromLibraryNameId :: LibraryNameId -> Int32
fromLibraryNameId (LibraryNameId x) = x
