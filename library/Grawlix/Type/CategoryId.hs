{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.CategoryId
  ( CategoryId
  , toCategoryId
  , fromCategoryId
  ) where

import qualified Data.Int as Int

newtype CategoryId = CategoryId Int.Int32
  deriving (Eq, Show)

toCategoryId :: Int.Int32 -> CategoryId
toCategoryId = CategoryId

fromCategoryId :: CategoryId -> Int.Int32
fromCategoryId (CategoryId x) = x
