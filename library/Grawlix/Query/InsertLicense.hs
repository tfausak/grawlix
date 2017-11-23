{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertLicense
  ( insertLicense
  ) where

import Grawlix.Query.Common
import Grawlix.Type.License

import qualified Hasql.Decoders as D

insertLicense :: Query License ()
insertLicense =
  makeQuery
    [string|
      insert into licenses ( content )
      values ( $1 )
      on conflict do nothing
    |]
    (contramap fromLicense encodeText)
    D.unit
