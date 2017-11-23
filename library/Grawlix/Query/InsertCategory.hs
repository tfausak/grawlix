module Grawlix.Query.InsertCategory
  ( insertCategory
  ) where

import Grawlix.Query.Common
import Grawlix.Type.Category

insertCategory :: Query Category ()
insertCategory =
  makeQuery
    " insert into categories ( content ) \
    \ values ( $1 ) \
    \ on conflict do nothing "
    (contramap fromCategory encodeText)
    decodeUnit
