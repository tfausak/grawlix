{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.PackageUrl
  ( PackageUrl
  , toPackageUrl
  , fromPackageUrl
  ) where

import qualified Data.Text as Text

newtype PackageUrl = PackageUrl Text.Text
  deriving (Eq, Show)

toPackageUrl :: Text.Text -> PackageUrl
toPackageUrl = PackageUrl

fromPackageUrl :: PackageUrl -> Text.Text
fromPackageUrl (PackageUrl x) = x
