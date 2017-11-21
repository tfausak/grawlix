{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.TestNameId
  ( TestNameId
  , toTestNameId
  , fromTestNameId
  ) where

import Grawlix.Type.Common

newtype TestNameId = TestNameId Int32
  deriving (Eq, Show)

toTestNameId :: Int32 -> TestNameId
toTestNameId = TestNameId

fromTestNameId :: TestNameId -> Int32
fromTestNameId (TestNameId x) = x
