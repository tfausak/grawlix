{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.ExecutableNameId
  ( ExecutableNameId
  , toExecutableNameId
  , fromExecutableNameId
  ) where

import qualified Data.Int as Int

newtype ExecutableNameId = ExecutableNameId Int.Int32
  deriving (Eq, Show)

toExecutableNameId :: Int.Int32 -> ExecutableNameId
toExecutableNameId = ExecutableNameId

fromExecutableNameId :: ExecutableNameId -> Int.Int32
fromExecutableNameId (ExecutableNameId x) = x
