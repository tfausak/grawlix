module Grawlix.Database
  ( getConnection
  , runMigrations
  , runQuery
  ) where

import Grawlix.Query.Common
import Grawlix.Type.Config

import qualified Data.Text.Encoding as Text
import qualified Hasql.Connection as Sql
import qualified Hasql.Migration as Sql
import qualified Hasql.Session as Sql
import qualified Hasql.Transaction as Sql.Transaction
import qualified Hasql.Transaction.Sessions as Sql

getConnection :: Config -> IO Sql.Connection
getConnection config = do
  result <- Sql.acquire . Text.encodeUtf8 $ configPostgresUri config
  case result of
    Left problem -> fail $ show problem
    Right connection -> pure connection

runMigrations :: Config -> Sql.Connection -> IO ()
runMigrations config connection = do
  runMigration connection Sql.MigrationInitialization
  migrations <- getMigrations config
  mapM_ (runMigration connection) migrations

getMigrations :: Config -> IO [Sql.MigrationCommand]
getMigrations = Sql.loadMigrationsFromDirectory . configMigrationDirectory

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
