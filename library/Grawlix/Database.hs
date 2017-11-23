module Grawlix.Database
  ( getConnection
  , runMigration
  , runQuery
  ) where

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

getConnection :: IO Sql.Connection
getConnection = do
  db <- Environment.lookupEnv "DATABASE"
  result <- Sql.acquire . Text.encodeUtf8 . Text.pack $ Maybe.fromMaybe "" db
  case result of
    Left problem -> fail $ show problem
    Right connection -> pure connection

runMigration :: Sql.Connection -> Sql.MigrationCommand -> IO ()
runMigration connection migration = do
  result <- Sql.run (transaction $ Sql.runMigration migration) connection
  case result of
    Right Sql.MigrationSuccess -> pure ()
    _ -> fail (show result)

runQuery :: Sql.Connection -> Query a b -> a -> IO b
runQuery connection query params = do
  result <- Sql.run (Sql.query params query) connection
  case result of
    Left problem -> fail $ show problem
    Right value -> pure value

transaction :: Sql.Transaction.Transaction a -> Sql.Session a
transaction =
  Sql.transaction Sql.Transaction.Serializable Sql.Transaction.Write
