{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Constraint
  ( Constraint
  , toConstraint
  , fromConstraint
  ) where

import qualified Data.Text as Text

newtype Constraint = Constraint Text.Text
  deriving (Eq, Ord, Show)

toConstraint :: Text.Text -> Constraint
toConstraint = Constraint

fromConstraint :: Constraint -> Text.Text
fromConstraint (Constraint x) = x
