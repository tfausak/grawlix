module Grawlix.Type.PackageId
  ( PackageId
  , toPackageId
  , fromPackageId
  ) where

import Grawlix.Type.Common

newtype PackageId =
  PackageId Int32
  deriving (Eq, Show)

toPackageId :: Int32 -> PackageId
toPackageId = PackageId

fromPackageId :: PackageId -> Int32
fromPackageId (PackageId x) = x
