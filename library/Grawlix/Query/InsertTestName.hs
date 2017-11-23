{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertTestName
  ( insertTestName
  ) where

import Grawlix.Query.Common
import Grawlix.Type.TestName

import qualified Hasql.Decoders as D

insertTestName :: Query TestName ()
insertTestName =
  makeQuery
    [string|
      insert into test_names ( content )
      values ( $1 )
      on conflict do nothing
    |]
    (contramap fromTestName encodeText)
    D.unit
