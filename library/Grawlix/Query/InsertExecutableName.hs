{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertExecutableName
  ( insertExecutableName
  ) where

import Grawlix.Query.Common
import Grawlix.Type.ExecutableName

import qualified Hasql.Decoders as D

insertExecutableName :: Query ExecutableName ()
insertExecutableName =
  makeQuery
    [string|
      insert into executable_names ( content )
      values ( $1 )
      on conflict do nothing
    |]
    (contramap fromExecutableName encodeText)
    D.unit
