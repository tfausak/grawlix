{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.InsertDependencyExecutable
  ( insertDependencyExecutable
  ) where

import Grawlix.Query.Common
import Grawlix.Type.DependencyId
import Grawlix.Type.ExecutableId

import qualified Hasql.Decoders as D

insertDependencyExecutable :: Query (DependencyId, ExecutableId) ()
insertDependencyExecutable =
  makeQuery
    [string|
      insert into dependencies_executables ( dependency_id, executable_id )
      values ( $1, $2 )
      on conflict do nothing
    |]
    (contrazip2
       (contramap fromDependencyId encodeInt32)
       (contramap fromExecutableId encodeInt32))
    D.unit
