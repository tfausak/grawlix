{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertCondition
  ( insertCondition
  ) where

import Grawlix.Query.Common
import Grawlix.Type.Condition

import qualified Hasql.Decoders as D

insertCondition :: Query Condition ()
insertCondition =
  makeQuery
    [string|
      insert into conditions ( content )
      values ( $1 )
      on conflict do nothing
    |]
    (contramap fromCondition encodeText)
    D.unit
