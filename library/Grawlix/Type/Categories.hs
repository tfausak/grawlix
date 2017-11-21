module Grawlix.Type.Categories
  ( Categories
  , toCategories
  , fromCategories
  ) where

import Grawlix.Type.Category
import Grawlix.Type.Common

newtype Categories =
  Categories (Set Category)
  deriving (Eq, Show)

toCategories :: Set Category -> Categories
toCategories = Categories

fromCategories :: Categories -> Set Category
fromCategories (Categories x) = x
