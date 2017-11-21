{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.PackageId
  ( PackageId
  , toPackageId
  , fromPackageId
  ) where

import qualified Data.Int as Int

newtype PackageId = PackageId Int.Int32
  deriving (Eq, Show)

toPackageId :: Int.Int32 -> PackageId
toPackageId = PackageId

fromPackageId :: PackageId -> Int.Int32
fromPackageId (PackageId x) = x
