{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Revision
  ( Revision
  , toRevision
  , fromRevision
  ) where

import qualified Data.Aeson as Json
import qualified Data.Int as Int
import qualified Web.HttpApiData as HttpApiData

newtype Revision = Revision Int.Int32
  deriving (Eq, HttpApiData.FromHttpApiData, Show, Json.ToJSON)

toRevision :: Int.Int32 -> Revision
toRevision = Revision

fromRevision :: Revision -> Int.Int32
fromRevision (Revision x) = x
