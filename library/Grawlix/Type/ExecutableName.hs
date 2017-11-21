{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.ExecutableName
  ( ExecutableName
  , toExecutableName
  , fromExecutableName
  ) where

import Grawlix.Type.Common

newtype ExecutableName = ExecutableName Text
  deriving (Eq, Ord, Show)

toExecutableName :: Text -> ExecutableName
toExecutableName = ExecutableName

fromExecutableName :: ExecutableName -> Text
fromExecutableName (ExecutableName x) = x
