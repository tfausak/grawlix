module Grawlix.Type.License
  ( License
  , toLicense
  , fromLicense
  ) where

import Grawlix.Type.Common

newtype License =
  License Text
  deriving (Eq, Show)

toLicense :: Text -> License
toLicense = License

fromLicense :: License -> Text
fromLicense (License x) = x
