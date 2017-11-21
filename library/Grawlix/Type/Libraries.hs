module Grawlix.Type.Libraries
  ( Libraries
  , toLibraries
  , fromLibraries
  ) where

import Grawlix.Type.Common
import Grawlix.Type.Library

newtype Libraries =
  Libraries (Set Library)
  deriving (Eq, Show)

toLibraries :: Set Library -> Libraries
toLibraries = Libraries

fromLibraries :: Libraries -> Set Library
fromLibraries (Libraries x) = x
