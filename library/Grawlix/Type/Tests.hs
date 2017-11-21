{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Tests
  ( Tests
  , toTests
  , fromTests
  ) where

import Grawlix.Type.Common
import Grawlix.Type.Test

newtype Tests = Tests (Set Test)
  deriving (Eq, Show)

toTests :: Set Test -> Tests
toTests = Tests

fromTests :: Tests -> Set Test
fromTests (Tests x) = x
