{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.PackageName
  ( PackageName
  , toPackageName
  , fromPackageName
  ) where

import Grawlix.Type.Common

newtype PackageName =
  PackageName Text
  deriving (Eq, FromHttpApiData, Ord, Show, ToJSON)

toPackageName :: Text -> PackageName
toPackageName = PackageName

fromPackageName :: PackageName -> Text
fromPackageName (PackageName x) = x
