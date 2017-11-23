{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.SelectExecutableNameId
  ( selectExecutableNameId
  ) where

import Grawlix.Query.Common
import Grawlix.Type.ExecutableName
import Grawlix.Type.ExecutableNameId

import qualified Hasql.Decoders as D

selectExecutableNameId :: Query ExecutableName ExecutableNameId
selectExecutableNameId =
  makeQuery
    [string|
      select id
      from executable_names
      where content = $1
    |]
    (contramap fromExecutableName encodeText)
    (toExecutableNameId <$> D.singleRow decodeInt32)
