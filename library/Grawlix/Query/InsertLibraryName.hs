module Grawlix.Query.InsertLibraryName
  ( insertLibraryName
  ) where

import Grawlix.Query.Common
import Grawlix.Type.LibraryName

insertLibraryName :: Query LibraryName ()
insertLibraryName =
  makeQuery
    " insert into library_names ( content ) \
    \ values ( $1 ) \
    \ on conflict do nothing "
    (contramap fromLibraryName encodeText)
    decodeUnit
