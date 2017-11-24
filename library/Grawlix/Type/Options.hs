{-# LANGUAGE DeriveGeneric #-}

module Grawlix.Type.Options
  ( Options(..)
  ) where

import Grawlix.Type.Common

import qualified Data.Aeson as Json
import qualified Options.Generic as Cli

newtype Options = Options
  { optionsConfigFile :: Maybe FilePath
  } deriving (Eq, Generic, Show)

instance Cli.ParseRecord Options where
  parseRecord =
    Cli.parseRecordWithModifiers
      Cli.defaultModifiers
      {Cli.fieldNameModifier = Json.camelTo2 '-' . partialDropPrefix "options"}
