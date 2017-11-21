{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.ConstraintId
  ( ConstraintId
  , toConstraintId
  , fromConstraintId
  ) where

import qualified Data.Int as Int

newtype ConstraintId = ConstraintId Int.Int32
  deriving (Eq, Show)

toConstraintId :: Int.Int32 -> ConstraintId
toConstraintId = ConstraintId

fromConstraintId :: ConstraintId -> Int.Int32
fromConstraintId (ConstraintId x) = x
