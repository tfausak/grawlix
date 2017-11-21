{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.LibraryId
  ( LibraryId
  , toLibraryId
  , fromLibraryId
  ) where

import Grawlix.Type.Common

newtype LibraryId =
  LibraryId Int32
  deriving (Eq, FromHttpApiData, Show, ToJSON)

toLibraryId :: Int32 -> LibraryId
toLibraryId = LibraryId

fromLibraryId :: LibraryId -> Int32
fromLibraryId (LibraryId x) = x
