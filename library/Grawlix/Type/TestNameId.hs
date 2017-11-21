{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.TestNameId
  ( TestNameId
  , toTestNameId
  , fromTestNameId
  ) where

import qualified Data.Int as Int

newtype TestNameId = TestNameId Int.Int32
  deriving (Eq, Show)

toTestNameId :: Int.Int32 -> TestNameId
toTestNameId = TestNameId

fromTestNameId :: TestNameId -> Int.Int32
fromTestNameId (TestNameId x) = x
