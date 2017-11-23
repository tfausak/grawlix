{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertPackageName
  ( insertPackageName
  ) where

import Grawlix.Query.Common
import Grawlix.Type.PackageName

insertPackageName :: Query PackageName ()
insertPackageName =
  makeQuery
    [string|
      insert into package_names ( content )
      values ( $1 )
      on conflict do nothing
    |]
    (contramap fromPackageName encodeText)
    decodeUnit
