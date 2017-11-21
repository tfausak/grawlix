module Grawlix.Type.Executables
  ( Executables
  , toExecutables
  , fromExecutables
  ) where

import Grawlix.Type.Common
import Grawlix.Type.Executable

newtype Executables =
  Executables (Set Executable)
  deriving (Eq, Show)

toExecutables :: Set Executable -> Executables
toExecutables = Executables

fromExecutables :: Executables -> Set Executable
fromExecutables (Executables x) = x
