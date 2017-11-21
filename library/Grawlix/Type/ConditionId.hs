{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.ConditionId
  ( ConditionId
  , toConditionId
  , fromConditionId
  ) where

import Grawlix.Type.Common

newtype ConditionId = ConditionId Int32
  deriving (Eq, Show)

toConditionId :: Int32 -> ConditionId
toConditionId = ConditionId

fromConditionId :: ConditionId -> Int32
fromConditionId (ConditionId x) = x
