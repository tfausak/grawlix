{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertCategory
  ( insertCategory
  ) where

import Grawlix.Query.Common
import Grawlix.Type.Category

import qualified Hasql.Decoders as D

insertCategory :: Query Category ()
insertCategory =
  makeQuery
    [string|
      insert into categories ( content )
      values ( $1 )
      on conflict do nothing
    |]
    (contramap fromCategory encodeText)
    D.unit
