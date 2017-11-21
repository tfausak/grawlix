{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Constraint
  ( Constraint
  , toConstraint
  , fromConstraint
  ) where

import Grawlix.Type.Common

newtype Constraint = Constraint Text
  deriving (Eq, Ord, Show)

toConstraint :: Text -> Constraint
toConstraint = Constraint

fromConstraint :: Constraint -> Text
fromConstraint (Constraint x) = x
