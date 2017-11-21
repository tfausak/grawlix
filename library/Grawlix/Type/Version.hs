{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Grawlix.Type.Version
  ( Version
  , toVersion
  , fromVersion
  ) where

import Grawlix.Type.Common

import qualified Control.Monad as Monad
import qualified Data.Text as Text
import qualified Distribution.Compat.ReadP as Cabal
import qualified Distribution.Text as Cabal
import qualified Distribution.Version as Cabal

newtype Version =
  Version [Int32]
  deriving (Eq, Show, ToJSON)

instance FromHttpApiData Version where
  parseUrlPiece =
    let fromList x =
          case x of
            [] -> fail "invalid version number"
            y:_ -> pure y
        toInt32 x =
          if x > fromIntegral (maxBound :: Int32)
            then fail "invalid version component"
            else pure (fromIntegral x)
    in fmap toVersion .
       Monad.join .
       fmap (mapM toInt32 . Cabal.versionNumbers) .
       fromList .
       map fst .
       filter (null . snd) . Cabal.readP_to_S Cabal.parse . Text.unpack

toVersion :: [Int32] -> Version
toVersion = Version

fromVersion :: Version -> [Int32]
fromVersion (Version x) = x
