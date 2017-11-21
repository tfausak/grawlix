{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.LibraryNameId
  ( LibraryNameId
  , toLibraryNameId
  , fromLibraryNameId
  ) where

import qualified Data.Int as Int

newtype LibraryNameId = LibraryNameId Int.Int32
  deriving (Eq, Show)

toLibraryNameId :: Int.Int32 -> LibraryNameId
toLibraryNameId = LibraryNameId

fromLibraryNameId :: LibraryNameId -> Int.Int32
fromLibraryNameId (LibraryNameId x) = x
