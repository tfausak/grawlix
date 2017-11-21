module Grawlix.Type.ConstraintId
  ( ConstraintId
  , toConstraintId
  , fromConstraintId
  ) where

import Grawlix.Type.Common

newtype ConstraintId =
  ConstraintId Int32
  deriving (Eq, Show)

toConstraintId :: Int32 -> ConstraintId
toConstraintId = ConstraintId

fromConstraintId :: ConstraintId -> Int32
fromConstraintId (ConstraintId x) = x
