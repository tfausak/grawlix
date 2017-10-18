{-# LANGUAGE DataKinds #-}

module Grawlix.Types where

import qualified Data.Int as Int
import qualified Data.Map as Map
import qualified Data.Tagged as Tagged
import qualified Data.Text as Text
import qualified Distribution.Version as Cabal


data Package = Package
  { packageName :: PackageName
  , packageVersion :: Version
  , packageRevision :: Revision
  , packageLicense :: License
  , packageSynopsis :: Text.Text
  , packageDescription :: Text.Text
  , packageCategories :: [Category]
  , packageUrl :: Text.Text
  , packageRepos :: [Repo]
  , packageLibraries :: [Library]
  , packageExecutables :: [Executable]
  , packageTests :: [Test]
  , packageBenchmarks :: [Benchmark]
  } deriving Show


data Repo = Repo
  { repoKind :: RepoKind
  , repoType :: RepoType
  , repoUrl :: Text.Text
  } deriving Show


data Library = Library
  { libraryName :: LibraryName
  , libraryCondition :: Condition
  , libraryModules :: [ModuleName]
  , libraryDependencies :: Dependencies
  } deriving Show


data Dependency = Dependency
  { dependencyPackage :: PackageName
  , dependencyVersionRange :: Cabal.VersionRange
  } deriving Show


data Executable = Executable
  { executableName :: ExecutableName
  , executableCondition :: Condition
  , executableDependencies :: Dependencies
  } deriving Show


data Test = Test
  { testName :: TestName
  , testCondition :: Condition
  , testDependencies :: Dependencies
  } deriving Show


data Benchmark = Benchmark
  { benchmarkName :: BenchmarkName
  , benchmarkCondition :: Condition
  , benchmarkDependencies :: Dependencies
  } deriving Show


type BenchmarkName = Tagged.Tagged "BenchmarkName" Text.Text
type Category = Tagged.Tagged "Category" Text.Text
type CategoryId = Tagged.Tagged "CategoryId" Int.Int32
type Condition = Tagged.Tagged "Condition" Text.Text
type ConditionId = Tagged.Tagged "ConditionId" Int.Int32
type Constraint = Tagged.Tagged "Constraint" Text.Text
type ConstraintId = Tagged.Tagged "ConstraintId" Int.Int32
type Dependencies = Map.Map PackageName Constraint
type DependencyId = Tagged.Tagged "DependencyId" Int.Int32
type ExecutableId = Tagged.Tagged "ExecutableId" Int.Int32
type ExecutableName = Tagged.Tagged "ExecutableName" Text.Text
type ExecutableNameId = Tagged.Tagged "ExecutableNameId" Int.Int32
type LibraryId = Tagged.Tagged "LibraryId" Int.Int32
type LibraryName = Tagged.Tagged "LibraryName" Text.Text
type LibraryNameId = Tagged.Tagged "LibraryNameId" Int.Int32
type License = Tagged.Tagged "License" Text.Text
type ModuleName = Tagged.Tagged "ModuleName" [Text.Text]
type ModuleNameId = Tagged.Tagged "ModuleNameId" Int.Int32
type PackageId = Tagged.Tagged "PackageId" Int.Int32
type PackageName = Tagged.Tagged "PackageName" Text.Text
type PackageNameId = Tagged.Tagged "PackageNameId" Int.Int32
type RepoId = Tagged.Tagged "RepoId" Int.Int32
type RepoKind = Tagged.Tagged "RepoKind" Text.Text
type RepoKindId = Tagged.Tagged "RepoKindId" Int.Int32
type RepoType = Tagged.Tagged "RepoType" Text.Text
type RepoTypeId = Tagged.Tagged "RepoTypeId" Int.Int32
type Revision = Tagged.Tagged "Revision" Int.Int32
type TestName = Tagged.Tagged "TestName" Text.Text
type Version = Tagged.Tagged "Version" [Int.Int32]
