{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Tests
  ( Tests
  , toTests
  , fromTests
  ) where

import Grawlix.Type.Test

import qualified Data.Set as Set

newtype Tests = Tests (Set.Set Test)
  deriving (Eq, Show)

toTests :: Set.Set Test -> Tests
toTests = Tests

fromTests :: Tests -> Set.Set Test
fromTests (Tests x) = x
