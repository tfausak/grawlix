{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.CategoryId
  ( CategoryId
  , toCategoryId
  , fromCategoryId
  ) where

import Grawlix.Type.Common

newtype CategoryId = CategoryId Int32
  deriving (Eq, Show)

toCategoryId :: Int32 -> CategoryId
toCategoryId = CategoryId

fromCategoryId :: CategoryId -> Int32
fromCategoryId (CategoryId x) = x
