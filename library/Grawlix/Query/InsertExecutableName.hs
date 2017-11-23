{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertExecutableName
  ( insertExecutableName
  ) where

import Grawlix.Query.Common
import Grawlix.Type.ExecutableName

insertExecutableName :: Query ExecutableName ()
insertExecutableName =
  makeQuery
    [string|
      insert into executable_names ( content )
      values ( $1 )
      on conflict do nothing
    |]
    (contramap fromExecutableName encodeText)
    decodeUnit
