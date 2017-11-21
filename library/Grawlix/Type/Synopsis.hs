{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Synopsis
  ( Synopsis
  , toSynopsis
  , fromSynopsis
  ) where

import qualified Data.Text as Text

newtype Synopsis = Synopsis Text.Text
  deriving (Eq, Show)

toSynopsis :: Text.Text -> Synopsis
toSynopsis = Synopsis

fromSynopsis :: Synopsis -> Text.Text
fromSynopsis (Synopsis x) = x
