module Grawlix.Query.InsertCondition
  ( insertCondition
  ) where

import Grawlix.Query.Common
import Grawlix.Type.Condition

insertCondition :: Query Condition ()
insertCondition =
  makeQuery
    " insert into conditions ( content ) \
    \ values ( $1 ) \
    \ on conflict do nothing "
    (contramap fromCondition encodeText)
    decodeUnit
