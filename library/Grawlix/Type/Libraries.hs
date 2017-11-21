{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Libraries
  ( Libraries
  , toLibraries
  , fromLibraries
  ) where

import Grawlix.Type.Library

import qualified Data.Set as Set

newtype Libraries = Libraries (Set.Set Library)
  deriving (Eq, Show)

toLibraries :: Set.Set Library -> Libraries
toLibraries = Libraries

fromLibraries :: Libraries -> Set.Set Library
fromLibraries (Libraries x) = x
