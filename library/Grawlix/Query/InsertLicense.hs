{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertLicense
  ( insertLicense
  ) where

import Grawlix.Query.Common
import Grawlix.Type.License

insertLicense :: Query License ()
insertLicense =
  makeQuery
    [string|
      insert into licenses ( content )
      values ( $1 )
      on conflict do nothing
    |]
    (contramap fromLicense encodeText)
    decodeUnit
