{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Revision
  ( Revision
  , toRevision
  , fromRevision
  ) where

import Grawlix.Type.Common

newtype Revision =
  Revision Int32
  deriving (Eq, FromHttpApiData, Show, ToJSON)

toRevision :: Int32 -> Revision
toRevision = Revision

fromRevision :: Revision -> Int32
fromRevision (Revision x) = x
