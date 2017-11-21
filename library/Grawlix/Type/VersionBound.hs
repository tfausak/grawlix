module Grawlix.Type.VersionBound
  ( VersionBound
  , toVersionBound
  , fromVersionBound
  ) where

import qualified Distribution.Version as Cabal

newtype VersionBound =
  VersionBound Cabal.VersionRange
  deriving (Eq, Show)

toVersionBound :: Cabal.VersionRange -> VersionBound
toVersionBound = VersionBound

fromVersionBound :: VersionBound -> Cabal.VersionRange
fromVersionBound (VersionBound x) = x
