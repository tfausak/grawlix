{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.PackageName
  ( PackageName
  , toPackageName
  , fromPackageName
  ) where

import qualified Data.Aeson as Json
import qualified Data.Text as Text
import qualified Web.HttpApiData as HttpApiData

newtype PackageName = PackageName Text.Text
  deriving (Eq, HttpApiData.FromHttpApiData, Ord, Show, Json.ToJSON)

toPackageName :: Text.Text -> PackageName
toPackageName = PackageName

fromPackageName :: PackageName -> Text.Text
fromPackageName (PackageName x) = x
