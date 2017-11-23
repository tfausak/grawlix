{-# LANGUAGE QuasiQuotes #-}

module Grawlix.Query.SelectTrue
  ( selectTrue
  ) where

import Grawlix.Query.Common

import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E

selectTrue :: Query () Bool
selectTrue =
  makeQuery
    [string|
      select true
    |]
    E.unit
    (D.singleRow $ D.value D.bool)
