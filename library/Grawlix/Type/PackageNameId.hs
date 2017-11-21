{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.PackageNameId
  ( PackageNameId
  , toPackageNameId
  , fromPackageNameId
  ) where

import qualified Data.Int as Int

newtype PackageNameId = PackageNameId Int.Int32
  deriving (Eq, Show)

toPackageNameId :: Int.Int32 -> PackageNameId
toPackageNameId = PackageNameId

fromPackageNameId :: PackageNameId -> Int.Int32
fromPackageNameId (PackageNameId x) = x
