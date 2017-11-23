module Grawlix.Database
  ( module Grawlix.Database
  , module Grawlix.Query.InsertBenchmark
  , module Grawlix.Query.InsertBenchmarkName
  , module Grawlix.Query.InsertCategory
  , module Grawlix.Query.InsertCategoryPackage
  , module Grawlix.Query.InsertCondition
  , module Grawlix.Query.InsertConstraint
  , module Grawlix.Query.InsertDependency
  , module Grawlix.Query.InsertDependencyBenchmark
  , module Grawlix.Query.InsertDependencyExecutable
  , module Grawlix.Query.InsertDependencyLibrary
  , module Grawlix.Query.InsertDependencyTest
  , module Grawlix.Query.InsertExecutable
  , module Grawlix.Query.InsertExecutableName
  , module Grawlix.Query.InsertLibrary
  , module Grawlix.Query.InsertLibraryModuleName
  , module Grawlix.Query.InsertLibraryName
  , module Grawlix.Query.InsertLicense
  , module Grawlix.Query.InsertModuleName
  , module Grawlix.Query.InsertPackage
  , module Grawlix.Query.InsertPackageName
  , module Grawlix.Query.InsertPackageRepo
  , module Grawlix.Query.InsertRepo
  , module Grawlix.Query.InsertRepoKind
  , module Grawlix.Query.InsertRepoType
  , module Grawlix.Query.InsertTest
  , module Grawlix.Query.InsertTestName
  , module Grawlix.Query.InsertVersion
  , module Grawlix.Query.SelectBenchmarkId
  , module Grawlix.Query.SelectBenchmarkNameId
  , module Grawlix.Query.SelectCategoryId
  , module Grawlix.Query.SelectConditionId
  , module Grawlix.Query.SelectConstraintId
  , module Grawlix.Query.SelectDependencyId
  , module Grawlix.Query.SelectExecutableId
  , module Grawlix.Query.SelectExecutableNameId
  , module Grawlix.Query.SelectLibraries
  , module Grawlix.Query.SelectLibraryId
  , module Grawlix.Query.SelectLibraryNameId
  , module Grawlix.Query.SelectModuleNameId
  , module Grawlix.Query.SelectModules
  , module Grawlix.Query.SelectPackageId
  , module Grawlix.Query.SelectPackageNameId
  , module Grawlix.Query.SelectPackageNames
  , module Grawlix.Query.SelectRepoId
  , module Grawlix.Query.SelectRepoKindId
  , module Grawlix.Query.SelectRepoTypeId
  , module Grawlix.Query.SelectRevisions
  , module Grawlix.Query.SelectTestId
  , module Grawlix.Query.SelectTestNameId
  , module Grawlix.Query.SelectTrue
  , module Grawlix.Query.SelectVersions
  ) where

import Flow ((|>))
import Grawlix.Query.Common
import Grawlix.Query.InsertBenchmark
import Grawlix.Query.InsertBenchmarkName
import Grawlix.Query.InsertCategory
import Grawlix.Query.InsertCategoryPackage
import Grawlix.Query.InsertCondition
import Grawlix.Query.InsertConstraint
import Grawlix.Query.InsertDependency
import Grawlix.Query.InsertDependencyBenchmark
import Grawlix.Query.InsertDependencyExecutable
import Grawlix.Query.InsertDependencyLibrary
import Grawlix.Query.InsertDependencyTest
import Grawlix.Query.InsertExecutable
import Grawlix.Query.InsertExecutableName
import Grawlix.Query.InsertLibrary
import Grawlix.Query.InsertLibraryModuleName
import Grawlix.Query.InsertLibraryName
import Grawlix.Query.InsertLicense
import Grawlix.Query.InsertModuleName
import Grawlix.Query.InsertPackage
import Grawlix.Query.InsertPackageName
import Grawlix.Query.InsertPackageRepo
import Grawlix.Query.InsertRepo
import Grawlix.Query.InsertRepoKind
import Grawlix.Query.InsertRepoType
import Grawlix.Query.InsertTest
import Grawlix.Query.InsertTestName
import Grawlix.Query.InsertVersion
import Grawlix.Query.SelectBenchmarkId
import Grawlix.Query.SelectBenchmarkNameId
import Grawlix.Query.SelectCategoryId
import Grawlix.Query.SelectConditionId
import Grawlix.Query.SelectConstraintId
import Grawlix.Query.SelectDependencyId
import Grawlix.Query.SelectExecutableId
import Grawlix.Query.SelectExecutableNameId
import Grawlix.Query.SelectLibraries
import Grawlix.Query.SelectLibraryId
import Grawlix.Query.SelectLibraryNameId
import Grawlix.Query.SelectModuleNameId
import Grawlix.Query.SelectModules
import Grawlix.Query.SelectPackageId
import Grawlix.Query.SelectPackageNameId
import Grawlix.Query.SelectPackageNames
import Grawlix.Query.SelectRepoId
import Grawlix.Query.SelectRepoKindId
import Grawlix.Query.SelectRepoTypeId
import Grawlix.Query.SelectRevisions
import Grawlix.Query.SelectTestId
import Grawlix.Query.SelectTestNameId
import Grawlix.Query.SelectTrue
import Grawlix.Query.SelectVersions

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
