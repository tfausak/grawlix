{-# LANGUAGE DeriveGeneric #-}

module Grawlix.Type.Config
  ( Config(..)
  , defaultConfig
  ) where

import Grawlix.Type.Common

import qualified Data.Aeson as Json
import qualified Data.Aeson.Types as Json
import qualified Data.Text as Text
import qualified System.FilePath as Path

data Config = Config
  { configCacheDirectory :: FilePath
  , configIndexUrl :: String
  , configMigrationDirectory :: FilePath
  , configPostgresUri :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON Config where
  parseJSON =
    Json.withObject "Config" $ \object ->
      Config <$> getWithDefault object "cache-directory" configCacheDirectory <*>
      getWithDefault object "index-url" configIndexUrl <*>
      getWithDefault object "migration-directory" configMigrationDirectory <*>
      getWithDefault object "postgres-uri" configPostgresUri

getWithDefault ::
     FromJSON a => Json.Object -> String -> (Config -> a) -> Json.Parser a
getWithDefault object key field =
  object Json..:? Text.pack key Json..!= field defaultConfig

defaultConfig :: Config
defaultConfig =
  Config
  { configCacheDirectory = Path.combine "data" "cache"
  , configIndexUrl = "https://hackage.haskell.org/01-index.tar.gz"
  , configMigrationDirectory = "migrations"
  , configPostgresUri = Text.empty
  }
