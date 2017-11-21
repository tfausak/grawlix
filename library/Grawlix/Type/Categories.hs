{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Categories
  ( Categories
  , toCategories
  , fromCategories
  ) where

import Grawlix.Type.Category

import qualified Data.Set as Set

newtype Categories = Categories (Set.Set Category)
  deriving (Eq, Show)

toCategories :: Set.Set Category -> Categories
toCategories = Categories

fromCategories :: Categories -> Set.Set Category
fromCategories (Categories x) = x
