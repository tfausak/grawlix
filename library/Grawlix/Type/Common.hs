module Grawlix.Type.Common
  ( FromHttpApiData
  , FromJSON
  , Generic
  , Int32
  , Map
  , Set
  , Text
  , ToJSON
  , parseUrlPiece
  , partialDropPrefix
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData, parseUrlPiece)

import qualified Data.Maybe as Maybe
import qualified GHC.Stack as Ghc

dropPrefix :: Eq a => [a] -> [a] -> Maybe [a]
dropPrefix prefix list =
  case prefix of
    [] -> Just list
    ph:pt ->
      case list of
        [] -> Nothing
        lh:lt ->
          if ph == lh
            then dropPrefix pt lt
            else Nothing

partialDropPrefix :: (Ghc.HasCallStack, Eq a, Show a) => [a] -> [a] -> [a]
partialDropPrefix prefix list =
  Maybe.fromMaybe
    (error $ unwords [show prefix, "is not a prefix of", show list])
    (dropPrefix prefix list)
