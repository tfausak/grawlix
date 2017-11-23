{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertConstraint
  ( insertConstraint
  ) where

import Grawlix.Query.Common
import Grawlix.Type.Constraint

import qualified Hasql.Decoders as D

insertConstraint :: Query Constraint ()
insertConstraint =
  makeQuery
    [string|
      insert into constraints ( content )
      values ( $1 )
      on conflict do nothing
    |]
    (contramap fromConstraint encodeText)
    D.unit
