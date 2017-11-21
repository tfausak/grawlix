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
    fmap toVersion .
    Monad.join .
    fmap (mapM intToInt32 . Cabal.versionNumbers) .
    safeHead .
    map fst . filter (null . snd) . Cabal.readP_to_S Cabal.parse . Text.unpack

toVersion :: [Int32] -> Version
toVersion = Version

fromVersion :: Version -> [Int32]
fromVersion (Version x) = x

safeHead :: Monad m => [a] -> m a
safeHead x =
  case x of
    [] -> fail "empty list"
    y:_ -> pure y

intToInt32 :: Monad m => Int -> m Int32
intToInt32 x =
  if x > fromIntegral (maxBound :: Int32)
    then fail "too big for int32"
    else pure (fromIntegral x)
