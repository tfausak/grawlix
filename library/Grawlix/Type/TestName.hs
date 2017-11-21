{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.TestName
  ( TestName
  , toTestName
  , fromTestName
  ) where

import qualified Data.Text as Text

newtype TestName = TestName Text.Text
  deriving (Eq, Ord, Show)

toTestName :: Text.Text -> TestName
toTestName = TestName

fromTestName :: TestName -> Text.Text
fromTestName (TestName x) = x
