{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertTestName
  ( insertTestName
  ) where

import Grawlix.Query.Common
import Grawlix.Type.TestName

insertTestName :: Query TestName ()
insertTestName =
  makeQuery
    [string|
      insert into test_names ( content )
      values ( $1 )
      on conflict do nothing
    |]
    (contramap fromTestName encodeText)
    decodeUnit
