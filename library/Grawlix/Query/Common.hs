module Grawlix.Query.Common
  ( Query
  , contramap
  , contrazip2
  , contrazip3
  , contrazip4
  , contrazip5
  , contrazip6
  , contrazip7
  , decodeInt32
  , decodeList
  , decodeText
  , decodeUnit
  , encodeInt32
  , encodeList
  , encodeText
  , encodeUnit
  , makeQuery
  ) where

import Contravariant.Extras
  ( contrazip2
  , contrazip3
  , contrazip4
  , contrazip5
  , contrazip6
  , contrazip7
  )
import Control.Monad (replicateM)
import Data.Functor.Contravariant (contramap)
import Data.Int (Int32)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Hasql.Query (Query, statement)

import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

decodeInt32 :: D.Row Int32
decodeInt32 = D.value D.int4

decodeList :: D.Value a -> D.Row [a]
decodeList = D.value . D.array . D.arrayDimension replicateM . D.arrayValue

decodeText :: D.Row Text
decodeText = D.value D.text

decodeUnit :: D.Result ()
decodeUnit = D.unit

encodeInt32 :: E.Params Int32
encodeInt32 = E.value E.int4

encodeList :: E.Value a -> E.Params [a]
encodeList = E.value . E.array . E.arrayDimension foldl . E.arrayValue

encodeText :: E.Params Text
encodeText = E.value E.text

encodeUnit :: E.Params ()
encodeUnit = E.unit

makeQuery :: String -> E.Params a -> D.Result b -> Query a b
makeQuery query params result =
  statement (encodeUtf8 $ pack query) params result True
