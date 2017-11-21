{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.ExecutableName
  ( ExecutableName
  , toExecutableName
  , fromExecutableName
  ) where

import qualified Data.Text as Text

newtype ExecutableName = ExecutableName Text.Text
  deriving (Eq, Ord, Show)

toExecutableName :: Text.Text -> ExecutableName
toExecutableName = ExecutableName

fromExecutableName :: ExecutableName -> Text.Text
fromExecutableName (ExecutableName x) = x
