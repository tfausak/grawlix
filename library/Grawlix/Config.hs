module Grawlix.Config
  ( getConfig
  ) where

import Grawlix.Type.Config
import Grawlix.Type.Options

import qualified Data.Yaml as Yaml

getConfig :: Options -> IO Config
getConfig options =
  case optionsConfigFile options of
    Nothing -> pure defaultConfig
    Just file -> do
      result <- Yaml.decodeFileEither file
      case result of
        Left problem -> fail $ Yaml.prettyPrintParseException problem
        Right config -> pure config
