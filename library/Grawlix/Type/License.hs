{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.License
  ( License
  , toLicense
  , fromLicense
  ) where

import qualified Data.Text as Text

newtype License = License Text.Text
  deriving (Eq, Show)

toLicense :: Text.Text -> License
toLicense = License

fromLicense :: License -> Text.Text
fromLicense (License x) = x
