{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertPackageName
  ( insertPackageName
  ) where

import Grawlix.Query.Common
import Grawlix.Type.PackageName

import qualified Hasql.Decoders as D

insertPackageName :: Query PackageName ()
insertPackageName =
  makeQuery
    [string|
      insert into package_names ( content )
      values ( $1 )
      on conflict do nothing
    |]
    (contramap fromPackageName encodeText)
    D.unit
