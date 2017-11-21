{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Category
  ( Category
  , toCategory
  , fromCategory
  ) where

import qualified Data.Text as Text

newtype Category = Category Text.Text
  deriving (Eq, Ord, Show)

toCategory :: Text.Text -> Category
toCategory = Category

fromCategory :: Category -> Text.Text
fromCategory (Category x) = x
