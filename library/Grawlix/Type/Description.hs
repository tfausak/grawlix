{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Description
  ( Description
  , toDescription
  , fromDescription
  ) where

import Grawlix.Type.Common

newtype Description = Description Text
  deriving (Eq, Show)

toDescription :: Text -> Description
toDescription = Description

fromDescription :: Description -> Text
fromDescription (Description x) = x
