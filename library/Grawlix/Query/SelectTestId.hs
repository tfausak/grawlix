module Grawlix.Query.SelectTestId
  ( selectTestId
  ) where

import Grawlix.Query.Common
import Grawlix.Type.ConditionId
import Grawlix.Type.PackageId
import Grawlix.Type.TestId
import Grawlix.Type.TestNameId

import qualified Hasql.Decoders as D

selectTestId :: Query (PackageId, TestNameId, ConditionId) TestId
selectTestId =
  makeQuery
    " select id \
    \ from tests \
    \ where package_id = $1 \
    \ and test_name_id = $2 \
    \ and condition_id = $3 "
    (contrazip3
       (contramap fromPackageId encodeInt32)
       (contramap fromTestNameId encodeInt32)
       (contramap fromConditionId encodeInt32))
    (toTestId <$> D.singleRow decodeInt32)
