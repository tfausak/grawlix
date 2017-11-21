{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Executables
  ( Executables
  , toExecutables
  , fromExecutables
  ) where

import Grawlix.Type.Executable

import qualified Data.Set as Set

newtype Executables = Executables (Set.Set Executable)
  deriving (Eq, Show)

toExecutables :: Set.Set Executable -> Executables
toExecutables = Executables

fromExecutables :: Executables -> Set.Set Executable
fromExecutables (Executables x) = x
