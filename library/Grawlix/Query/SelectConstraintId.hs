{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.SelectConstraintId
  ( selectConstraintId
  ) where

import Grawlix.Query.Common
import Grawlix.Type.Constraint
import Grawlix.Type.ConstraintId

import qualified Hasql.Decoders as D

selectConstraintId :: Query Constraint ConstraintId
selectConstraintId =
  makeQuery
    [string|
      select id
      from constraints
      where content = $1
    |]
    (contramap fromConstraint encodeText)
    (toConstraintId <$> D.singleRow decodeInt32)
