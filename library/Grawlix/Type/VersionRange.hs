{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.VersionRange
  ( VersionRange
  , toVersionRange
  , fromVersionRange
  ) where

import qualified Distribution.Version as Cabal

newtype VersionRange = VersionRange Cabal.VersionRange
  deriving (Eq, Show)

toVersionRange :: Cabal.VersionRange -> VersionRange
toVersionRange = VersionRange

fromVersionRange :: VersionRange -> Cabal.VersionRange
fromVersionRange (VersionRange x) = x
