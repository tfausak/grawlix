module Grawlix.Type.Synopsis
  ( Synopsis
  , toSynopsis
  , fromSynopsis
  ) where

import Grawlix.Type.Common

newtype Synopsis =
  Synopsis Text
  deriving (Eq, Show)

toSynopsis :: Text -> Synopsis
toSynopsis = Synopsis

fromSynopsis :: Synopsis -> Text
fromSynopsis (Synopsis x) = x
