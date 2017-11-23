{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertVersion
  ( insertVersion
  ) where

import Grawlix.Query.Common
import Grawlix.Type.Version

import qualified Hasql.Encoders as E

insertVersion :: Query Version ()
insertVersion =
  makeQuery
    [string|
      insert into versions ( content )
      values ( $1 )
      on conflict do nothing
    |]
    (contramap fromVersion $ encodeList E.int4)
    decodeUnit
