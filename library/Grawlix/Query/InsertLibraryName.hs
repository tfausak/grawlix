{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertLibraryName
  ( insertLibraryName
  ) where

import Grawlix.Query.Common
import Grawlix.Type.LibraryName

import qualified Hasql.Decoders as D

insertLibraryName :: Query LibraryName ()
insertLibraryName =
  makeQuery
    [string|
      insert into library_names ( content )
      values ( $1 )
      on conflict do nothing
    |]
    (contramap fromLibraryName encodeText)
    D.unit
