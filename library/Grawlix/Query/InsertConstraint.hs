module Grawlix.Query.InsertConstraint
  ( insertConstraint
  ) where

import Grawlix.Query.Common
import Grawlix.Type.Constraint

insertConstraint :: Query Constraint ()
insertConstraint =
  makeQuery
    " insert into constraints ( content ) \
    \ values ( $1 ) \
    \ on conflict do nothing "
    (contramap fromConstraint encodeText)
    decodeUnit
