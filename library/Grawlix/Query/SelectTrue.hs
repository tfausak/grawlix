module Grawlix.Query.SelectTrue
  ( selectTrue
  ) where

import Grawlix.Query.Common

import qualified Hasql.Decoders as D

selectTrue :: Query () Bool
selectTrue = makeQuery "select true" encodeUnit (D.singleRow $ D.value D.bool)
