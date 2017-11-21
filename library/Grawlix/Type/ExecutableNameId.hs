module Grawlix.Type.ExecutableNameId
  ( ExecutableNameId
  , toExecutableNameId
  , fromExecutableNameId
  ) where

import Grawlix.Type.Common

newtype ExecutableNameId =
  ExecutableNameId Int32
  deriving (Eq, Show)

toExecutableNameId :: Int32 -> ExecutableNameId
toExecutableNameId = ExecutableNameId

fromExecutableNameId :: ExecutableNameId -> Int32
fromExecutableNameId (ExecutableNameId x) = x
