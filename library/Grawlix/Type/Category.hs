{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Category
  ( Category
  , toCategory
  , fromCategory
  ) where

import Grawlix.Type.Common

newtype Category = Category Text
  deriving (Eq, Ord, Show)

toCategory :: Text -> Category
toCategory = Category

fromCategory :: Category -> Text
fromCategory (Category x) = x
