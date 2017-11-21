{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.TestId
  ( TestId
  , toTestId
  , fromTestId
  ) where

import qualified Data.Int as Int

newtype TestId = TestId Int.Int32
  deriving (Eq, Show)

toTestId :: Int.Int32 -> TestId
toTestId = TestId

fromTestId :: TestId -> Int.Int32
fromTestId (TestId x) = x
