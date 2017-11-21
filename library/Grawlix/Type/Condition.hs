module Grawlix.Type.Condition
  ( Condition
  , toCondition
  , fromCondition
  ) where

import Grawlix.Type.Common

newtype Condition =
  Condition Text
  deriving (Eq, Ord, Show)

toCondition :: Text -> Condition
toCondition = Condition

fromCondition :: Condition -> Text
fromCondition (Condition x) = x
