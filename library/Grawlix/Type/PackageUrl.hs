module Grawlix.Type.PackageUrl
  ( PackageUrl
  , toPackageUrl
  , fromPackageUrl
  ) where

import Grawlix.Type.Common

newtype PackageUrl =
  PackageUrl Text
  deriving (Eq, Show)

toPackageUrl :: Text -> PackageUrl
toPackageUrl = PackageUrl

fromPackageUrl :: PackageUrl -> Text
fromPackageUrl (PackageUrl x) = x
