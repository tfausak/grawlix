{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Condition
  ( Condition
  , toCondition
  , fromCondition
  ) where

import qualified Data.Text as Text

newtype Condition = Condition Text.Text
  deriving (Eq, Ord, Show)

toCondition :: Text.Text -> Condition
toCondition = Condition

fromCondition :: Condition -> Text.Text
fromCondition (Condition x) = x
