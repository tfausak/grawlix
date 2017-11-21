{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.TestId
  ( TestId
  , toTestId
  , fromTestId
  ) where

import Grawlix.Type.Common

newtype TestId = TestId Int32
  deriving (Eq, Show)

toTestId :: Int32 -> TestId
toTestId = TestId

fromTestId :: TestId -> Int32
fromTestId (TestId x) = x
