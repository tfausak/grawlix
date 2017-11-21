{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.ExecutableId
  ( ExecutableId
  , toExecutableId
  , fromExecutableId
  ) where

import Grawlix.Type.Common

newtype ExecutableId = ExecutableId Int32
  deriving (Eq, Show)

toExecutableId :: Int32 -> ExecutableId
toExecutableId = ExecutableId

fromExecutableId :: ExecutableId -> Int32
fromExecutableId (ExecutableId x) = x
