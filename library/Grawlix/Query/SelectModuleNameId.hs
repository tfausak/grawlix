module Grawlix.Query.SelectModuleNameId
  ( selectModuleNameId
  ) where

import Grawlix.Query.Common
import Grawlix.Type.ModuleName
import Grawlix.Type.ModuleNameId

import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

selectModuleNameId :: Query ModuleName ModuleNameId
selectModuleNameId =
  makeQuery
    " select id \
    \ from module_names \
    \ where content = $1 "
    (contramap fromModuleName $ encodeList E.text)
    (toModuleNameId <$> D.singleRow decodeInt32)
