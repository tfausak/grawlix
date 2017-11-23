module Grawlix.Database
  ( getConnection
  , runMigration
  , runQuery
  ) where

import Flow ((|>))
import Grawlix.Query.Common

import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Hasql.Connection as Sql
import qualified Hasql.Migration as Sql
import qualified Hasql.Session as Sql
import qualified Hasql.Transaction as Sql.Transaction
import qualified Hasql.Transaction.Sessions as Sql
import qualified System.Environment as Environment


runQuery :: Sql.Connection -> Query a b -> a -> IO b
runQuery connection query params = do
  let session = Sql.query params query
  result <- Sql.run session connection
  case result of
    Left problem -> fail (show problem)
    Right value -> pure value


runMigration :: Sql.Connection -> Sql.MigrationCommand -> IO ()
runMigration connection migration = do
  let session = migration
        |> Sql.runMigration
        |> Sql.transaction Sql.Transaction.Serializable Sql.Transaction.Write
  result <- Sql.run session connection
  case result of
    Right Sql.MigrationSuccess -> pure ()
    _ -> result |> show |> fail


getConnection :: IO Sql.Connection
getConnection = do
  maybeSettings <- Environment.lookupEnv "DATABASE"
  result <- maybeSettings
    |> Maybe.fromMaybe ""
    |> Text.pack
    |> Text.encodeUtf8
    |> Sql.acquire
  case result of
    Left problem -> problem |> show |> fail
    Right connection -> pure connection
