{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.SelectTestNameId
  ( selectTestNameId
  ) where

import Grawlix.Query.Common
import Grawlix.Type.TestName
import Grawlix.Type.TestNameId

import qualified Hasql.Decoders as D

selectTestNameId :: Query TestName TestNameId
selectTestNameId =
  makeQuery
    [string|
      select id
      from test_names
      where content = $1
    |]
    (contramap fromTestName encodeText)
    (toTestNameId <$> D.singleRow decodeInt32)
