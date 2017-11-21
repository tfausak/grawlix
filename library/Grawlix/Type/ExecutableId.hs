{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.ExecutableId
  ( ExecutableId
  , toExecutableId
  , fromExecutableId
  ) where

import qualified Data.Int as Int

newtype ExecutableId = ExecutableId Int.Int32
  deriving (Eq, Show)

toExecutableId :: Int.Int32 -> ExecutableId
toExecutableId = ExecutableId

fromExecutableId :: ExecutableId -> Int.Int32
fromExecutableId (ExecutableId x) = x
