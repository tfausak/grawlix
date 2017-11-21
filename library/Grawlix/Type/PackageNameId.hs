{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.PackageNameId
  ( PackageNameId
  , toPackageNameId
  , fromPackageNameId
  ) where

import Grawlix.Type.Common

newtype PackageNameId = PackageNameId Int32
  deriving (Eq, Show)

toPackageNameId :: Int32 -> PackageNameId
toPackageNameId = PackageNameId

fromPackageNameId :: PackageNameId -> Int32
fromPackageNameId (PackageNameId x) = x
