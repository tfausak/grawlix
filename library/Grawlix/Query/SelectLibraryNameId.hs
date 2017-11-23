{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.SelectLibraryNameId
  ( selectLibraryNameId
  ) where

import Grawlix.Query.Common
import Grawlix.Type.LibraryName
import Grawlix.Type.LibraryNameId

import qualified Hasql.Decoders as D

selectLibraryNameId :: Query LibraryName LibraryNameId
selectLibraryNameId =
  makeQuery
    [string|
      select id
      from library_names
      where content = $1
    |]
    (contramap fromLibraryName encodeText)
    (toLibraryNameId <$> D.singleRow decodeInt32)
