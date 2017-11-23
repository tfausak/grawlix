module Grawlix.Query.InsertModuleName
  ( insertModuleName
  ) where

import Grawlix.Query.Common
import Grawlix.Type.ModuleName

import qualified Hasql.Encoders as E

insertModuleName :: Query ModuleName ()
insertModuleName =
  makeQuery
    " insert into module_names ( content ) \
    \ values ( $1 ) \
    \ on conflict do nothing "
    (contramap fromModuleName $ encodeList E.text)
    decodeUnit
