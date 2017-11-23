module Grawlix.Query.SelectCategoryId
  ( selectCategoryId
  ) where

import Grawlix.Query.Common
import Grawlix.Type.Category
import Grawlix.Type.CategoryId

import qualified Hasql.Decoders as D

selectCategoryId :: Query Category CategoryId
selectCategoryId =
  makeQuery
    " select id \
    \ from categories \
    \ where content = $1 "
    (contramap fromCategory encodeText)
    (toCategoryId <$> D.singleRow decodeInt32)
