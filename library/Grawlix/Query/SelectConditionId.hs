{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.SelectConditionId
  ( selectConditionId
  ) where

import Grawlix.Query.Common
import Grawlix.Type.Condition
import Grawlix.Type.ConditionId

import qualified Hasql.Decoders as D

selectConditionId :: Query Condition ConditionId
selectConditionId =
  makeQuery
    [string|
      select id
      from conditions
      where content = $1
    |]
    (contramap fromCondition encodeText)
    (toConditionId <$> D.singleRow decodeInt32)
