{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Description
  ( Description
  , toDescription
  , fromDescription
  ) where

import qualified Data.Text as Text

newtype Description = Description Text.Text
  deriving (Eq, Show)

toDescription :: Text.Text -> Description
toDescription = Description

fromDescription :: Description -> Text.Text
fromDescription (Description x) = x
