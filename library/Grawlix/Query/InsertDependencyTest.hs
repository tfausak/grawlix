module Grawlix.Query.InsertDependencyTest
  ( insertDependencyTest
  ) where

import Grawlix.Query.Common
import Grawlix.Type.DependencyId
import Grawlix.Type.TestId

insertDependencyTest :: Query (DependencyId, TestId) ()
insertDependencyTest =
  makeQuery
    " insert into dependencies_tests ( dependency_id, test_id ) \
    \ values ( $1, $2 ) \
    \ on conflict do nothing "
    (contrazip2
       (contramap fromDependencyId encodeInt32)
       (contramap fromTestId encodeInt32))
    decodeUnit
