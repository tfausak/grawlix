{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.LibraryId
  ( LibraryId
  , toLibraryId
  , fromLibraryId
  ) where

import qualified Data.Aeson as Json
import qualified Data.Int as Int
import qualified Web.HttpApiData as HttpApiData

newtype LibraryId = LibraryId Int.Int32
  deriving (Eq, HttpApiData.FromHttpApiData, Show, Json.ToJSON)

toLibraryId :: Int.Int32 -> LibraryId
toLibraryId = LibraryId

fromLibraryId :: LibraryId -> Int.Int32
fromLibraryId (LibraryId x) = x
