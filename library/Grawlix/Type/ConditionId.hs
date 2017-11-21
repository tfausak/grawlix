{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.ConditionId
  ( ConditionId
  , toConditionId
  , fromConditionId
  ) where

import qualified Data.Int as Int

newtype ConditionId = ConditionId Int.Int32
  deriving (Eq, Show)

toConditionId :: Int.Int32 -> ConditionId
toConditionId = ConditionId

fromConditionId :: ConditionId -> Int.Int32
fromConditionId (ConditionId x) = x
