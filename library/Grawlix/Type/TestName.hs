{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.TestName
  ( TestName
  , toTestName
  , fromTestName
  ) where

import Grawlix.Type.Common

newtype TestName = TestName Text
  deriving (Eq, Ord, Show)

toTestName :: Text -> TestName
toTestName = TestName

fromTestName :: TestName -> Text
fromTestName (TestName x) = x
