{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Grawlix.Sync (main) where

import Flow ((|>))
import Grawlix.Database
import Grawlix.Types

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as Gzip
import qualified Control.Arrow as Arrow
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Foldable as Foldable
import qualified Data.Functor as Functor
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Tagged as Tagged
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Tree as Tree
import qualified Data.Word as Word
import qualified Debug.Trace as Debug
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.Text as Cabal
import qualified Distribution.Types.CondTree as Cabal
import qualified Distribution.Types.UnqualComponentName as Cabal
import qualified Distribution.Version as Cabal
import qualified Hasql.Connection as Sql
import qualified Hasql.Migration as Sql
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Network.HTTP.Types as Http
import qualified System.Directory as Directory
import qualified System.FilePath as Path
import qualified System.IO as IO
import qualified Text.Printf as Printf
import qualified Text.Read as Read


main :: IO ()
main = do
  connection <- getConnection

  migrations <- Sql.loadMigrationsFromDirectory "migrations"
  migrations
    |> prepend Sql.MigrationInitialization
    |> mapM_ (runMigration connection)

  manager <- Client.newTlsManager
  md5 <- getLatestIndexMd5 manager
  let file = Path.addExtension md5 "tgz"
  index <- manager |> getLatestIndex |> getCached file
  index
    |> Gzip.decompress
    |> Tar.read
    |> fromEntries
    |> filter isCabal
    |> Maybe.mapMaybe (\ entry -> either
      (\ problem -> Debug.trace
        (Printf.printf "%s: %s" (Tar.entryPath entry) (show problem))
        Nothing)
      Just
      (do
        bytes <- getEntryContents entry
        text <- bytes |> fixEncoding |> lazyDecodeUtf8
        description <- text |> stripBom |> parseDescription
        toPackage description))
    |> mapM_ (handlePackage connection)


-- There is a single Cabal file in the entire Hackage index that is encoded
-- with ISO-8859-1 (also know as Latin-1 or Windows-1252) instead of UTF-8. It
-- contains a single 0xF6 byte (for U+00F6) that should be encoded as 0xC3 0xB6
-- instead. This function is responsible for fixing that single byte.
-- <https://hackage.haskell.org/package/nat-0.1/revision/0.cabal>
fixEncoding :: LazyBytes.ByteString -> LazyBytes.ByteString
fixEncoding bytes = LazyBytes.concatMap fixByte bytes


fixByte :: Word.Word8 -> LazyBytes.ByteString
fixByte byte = if byte == 0xf6
  then LazyBytes.pack [0xc3, 0xb6]
  else LazyBytes.singleton byte


-- There are a few Cabal files in the Hackage index that start with the UTF-16
-- byte order mark U+FEFF (0xFE 0xFF). Cabal is supposed to be able to handle
-- this, but it must do so somewhere other than its parser. This function is
-- responsible for removing the BOM at the start of the package description.
-- <https://hackage.haskell.org/package/Workflow-0.8.3/revision/0.cabal>
-- <https://hackage.haskell.org/package/dictionary-sharing-0.1.0.0/revision/0.cabal>
-- <https://hackage.haskell.org/package/testing-type-modifiers-0.1.0.0/revision/0.cabal>
stripBom :: LazyText.Text -> LazyText.Text
stripBom text = LazyText.dropWhile isBom text


isBom :: Char -> Bool
isBom char = char == '\xfeff'


handlePackage :: Sql.Connection -> Package -> IO ()
handlePackage connection package = do
  let name = packageName package
  let version = packageVersion package
  let revision = packageRevision package
  exists <- Exception.catch
    (do
      Functor.void
        (runQuery connection selectPackageId (name, version, revision))
      pure True)
    (\ (_ :: Exception.IOException) -> pure False)

  logPackage package exists

  Monad.unless exists (do
    runQuery connection insertPackageName name
    runQuery connection insertVersion version
    let license = packageLicense package
    runQuery connection insertLicense license

    runQuery connection insertPackage
      ( name
      , version
      , revision
      , license
      , packageSynopsis package
      , packageDescription package
      , packageUrl package
      )

    packageId <- runQuery connection selectPackageId (name, version, revision)

    package |> packageCategories |> mapM_ (\ category -> do
      runQuery connection insertCategory category
      categoryId <- runQuery connection selectCategoryId category
      runQuery connection insertCategoryPackage (categoryId, packageId))

    package |> packageRepos |> mapM_ (\ repo -> do
      let kind = repoKind repo
      runQuery connection insertRepoKind kind
      kindId <- runQuery connection selectRepoKindId kind

      let type_ = repoType repo
      runQuery connection insertRepoType type_
      typeId <- runQuery connection selectRepoTypeId type_

      let url = repoUrl repo
      runQuery connection insertRepo (kindId, typeId, url)
      repoId <- runQuery connection selectRepoId (kindId, typeId, url)
      runQuery connection insertPackageRepo (packageId, repoId))

    package |> packageLibraries |> mapM_ (\ library -> do
      let Library { libraryName, libraryCondition } = library

      runQuery connection insertLibraryName libraryName
      libraryNameId <- runQuery connection selectLibraryNameId libraryName

      runQuery connection insertCondition libraryCondition
      conditionId <- runQuery connection selectConditionId libraryCondition

      runQuery connection insertLibrary (packageId, libraryNameId, conditionId)
      libraryId <- runQuery connection selectLibraryId
        (packageId, libraryNameId, conditionId)

      library |> libraryModules |> mapM_ (\ moduleName -> do
        runQuery connection insertModuleName moduleName
        moduleNameId <- runQuery connection selectModuleNameId moduleName
        runQuery connection insertLibraryModuleName (libraryId, moduleNameId))

      library
        |> libraryDependencies
        |> Map.toAscList
        |> mapM_ (\ (packageName, constraint) -> do
          runQuery connection insertConstraint constraint
          constraintId <- runQuery connection selectConstraintId constraint

          runQuery connection insertPackageName packageName
          packageNameId <- runQuery connection selectPackageNameId packageName

          runQuery connection insertDependency (constraintId, packageNameId)
          dependencyId <- runQuery connection selectDependencyId
            (constraintId, packageNameId)
          runQuery connection insertDependencyLibrary
            (dependencyId, libraryId)))

    package |> packageExecutables |> mapM_ (\ executable -> do
      let Executable { executableName, executableCondition } = executable

      runQuery connection insertExecutableName executableName
      executableNameId <- runQuery connection selectExecutableNameId
        executableName

      runQuery connection insertCondition executableCondition
      conditionId <- runQuery connection selectConditionId executableCondition

      runQuery connection insertExecutable
        (packageId, executableNameId, conditionId)
      executableId <- runQuery connection selectExecutableId
        (packageId, executableNameId, conditionId)

      executable
        |> executableDependencies
        |> Map.toAscList
        |> mapM_ (\ (packageName, constraint) -> do
          runQuery connection insertConstraint constraint
          constraintId <- runQuery connection selectConstraintId constraint

          runQuery connection insertPackageName packageName
          packageNameId <- runQuery connection selectPackageNameId packageName

          runQuery connection insertDependency (constraintId, packageNameId)
          dependencyId <- runQuery connection selectDependencyId
            (constraintId, packageNameId)

          runQuery connection insertDependencyExecutable
            (dependencyId, executableId)))

    package |> packageTests |> mapM_ (\ test -> do
      let Test { testName, testCondition } = test

      runQuery connection insertTestName testName
      testNameId <- runQuery connection selectTestNameId testName

      runQuery connection insertCondition testCondition
      conditionId <- runQuery connection selectConditionId testCondition

      runQuery connection insertTest (packageId, testNameId, conditionId)
      testId <- runQuery connection selectTestId
        (packageId, testNameId, conditionId)

      test
        |> testDependencies
        |> Map.toAscList
        |> mapM_ (\ (packageName, constraint) -> do
          runQuery connection insertConstraint constraint
          constraintId <- runQuery connection selectConstraintId constraint

          runQuery connection insertPackageName packageName
          packageNameId <- runQuery connection selectPackageNameId packageName

          runQuery connection insertDependency (constraintId, packageNameId)
          dependencyId <- runQuery connection selectDependencyId
            (constraintId, packageNameId)

          runQuery connection insertDependencyTest (dependencyId, testId)))

    package |> packageBenchmarks |> mapM_ (\ benchmark -> do
      let Benchmark { benchmarkName, benchmarkCondition } = benchmark

      runQuery connection insertBenchmarkName benchmarkName
      benchmarkNameId <- runQuery connection selectBenchmarkNameId
        benchmarkName

      runQuery connection insertCondition benchmarkCondition
      conditionId <- runQuery connection selectConditionId benchmarkCondition

      runQuery connection insertBenchmark
        (packageId, benchmarkNameId, conditionId)
      benchmarkId <- runQuery connection selectBenchmarkId
        (packageId, benchmarkNameId, conditionId)

      benchmark
        |> benchmarkDependencies
        |> Map.toAscList
        |> mapM_ (\ (packageName, constraint) -> do
          runQuery connection insertConstraint constraint
          constraintId <- runQuery connection selectConstraintId constraint

          runQuery connection insertPackageName packageName
          packageNameId <- runQuery connection selectPackageNameId packageName

          runQuery connection insertDependency (constraintId, packageNameId)
          dependencyId <- runQuery connection selectDependencyId
            (constraintId, packageNameId)

          runQuery connection insertDependencyBenchmark
            (dependencyId, benchmarkId))))


logPackage :: Package -> Bool -> IO ()
logPackage package exists = Printf.printf "%s\t%s\t%d\t%s\n"
  (package |> packageName |> Tagged.untag |> Text.unpack)
  (package
    |> packageVersion
    |> Tagged.untag
    |> map show
    |> List.intercalate ".")
  (package |> packageRevision |> Tagged.untag)
  (if exists then "old" else "new")


toPackage :: Catch.MonadThrow m => Cabal.GenericPackageDescription -> m Package
toPackage package = do
  revision <- package
    |> Cabal.packageDescription
    |> Cabal.customFieldsPD
    |> lookup "x-revision"
    |> Maybe.fromMaybe "0"
    |> Read.readMaybe
    |> fromJust "failed to read revision"
  repos <- package
    |> Cabal.packageDescription
    |> Cabal.sourceRepos
    |> mapM toRepo
  pure Package
    { packageName = package
      |> Cabal.packageDescription
      |> Cabal.package
      |> Cabal.pkgName
      |> Cabal.unPackageName
      |> Text.pack
      |> Tagged.Tagged
    , packageVersion = package
      |> Cabal.packageDescription
      |> Cabal.package
      |> Cabal.pkgVersion
      |> Cabal.versionNumbers
      |> map intToInt32
      |> Tagged.Tagged
    , packageRevision = revision |> intToInt32 |> Tagged.Tagged
    , packageLicense = package
      |> Cabal.packageDescription
      |> Cabal.license
      |> Cabal.display
      |> Text.pack
      |> Tagged.Tagged
    , packageSynopsis = package
      |> Cabal.packageDescription
      |> Cabal.synopsis
      |> Text.pack
    , packageDescription = package
      |> Cabal.packageDescription
      |> Cabal.description
      |> Text.pack
    , packageCategories = package
      |> Cabal.packageDescription
      |> Cabal.category
      |> Text.pack
      |> Text.splitOn (Text.singleton ',')
      |> map Text.strip
      |> filter (\ x -> x |> Text.null |> not)
      |> map Tagged.Tagged
    , packageUrl = package
      |> Cabal.packageDescription
      |> Cabal.homepage
      |> Text.pack
    , packageRepos = repos
    , packageLibraries = let
      name = package
        |> Cabal.packageDescription
        |> Cabal.package
        |> Cabal.pkgName
        |> Cabal.unPackageName
        |> Text.pack
        |> Tagged.Tagged
      library = package
        |> Cabal.packageDescription
        |> Cabal.library
        |> Foldable.toList
        |> map withoutConditionsOrConstraints
      condLibrary = package
        |> Cabal.condLibrary
        |> Foldable.toList
        |> concatMap fromCondTree
      in [library, condLibrary] |> concat |> map (toLibrary name)
    , packageExecutables = let
      executables = package
        |> Cabal.packageDescription
        |> Cabal.executables
        |> map withoutConditionsOrConstraints
      condExecutables = package
        |> Cabal.condExecutables
        |> concatMap (\ (name, tree) -> tree
          |> fromCondTree
          |> map (Arrow.first (\ x -> x { Cabal.exeName = name })))
      in [executables, condExecutables] |> concat |> map toExecutable
    , packageTests = let
      tests = package
        |> Cabal.packageDescription
        |> Cabal.testSuites
        |> map withoutConditionsOrConstraints
      condTests = package
        |> Cabal.condTestSuites
        |> concatMap (\ (name, tree) -> tree
          |> fromCondTree
          |> map (Arrow.first (\ x -> x { Cabal.testName = name })))
      in [tests, condTests] |> concat |> map toTest
    , packageBenchmarks = let
      benchmarks = package
        |> Cabal.packageDescription
        |> Cabal.benchmarks
        |> map withoutConditionsOrConstraints
      condBenchmarks = package
        |> Cabal.condBenchmarks
        |> concatMap (\ (name, tree) -> tree
          |> fromCondTree
          |> map (Arrow.first (\ x -> x { Cabal.benchmarkName = name })))
      in [benchmarks, condBenchmarks] |> concat |> map toBenchmark
    }


toRepo :: Catch.MonadThrow m => Cabal.SourceRepo -> m Repo
toRepo repo = do
  let repoKind = repo
        |> Cabal.repoKind
        |> Cabal.display
        |> Text.pack
        |> Tagged.Tagged
  rawRepoType <- Cabal.repoType repo |> fromJust "could not get repo type"
  let repoType = rawRepoType
        |> Cabal.display
        |> Text.pack
        |> Tagged.Tagged
  rawRepoUrl <- Cabal.repoLocation repo |> fromJust "could not get repo URL"
  let repoUrl = rawRepoUrl |> Text.pack
  pure Repo { repoKind, repoType, repoUrl }


toLibrary
  :: LibraryName
  -> (Cabal.Library, ([Cabal.Condition Cabal.ConfVar], [Cabal.Dependency]))
  -> Library
toLibrary name (library, (conditions, constraints)) = Library
  { libraryName = name
  , libraryCondition = toCondition conditions
  , libraryModules = library
    |> Cabal.exposedModules
    |> map (\ moduleName -> moduleName
      |> Cabal.components
      |> map Text.pack
      |> Tagged.Tagged)
  , libraryDependencies = library
    |> Cabal.libBuildInfo
    |> Cabal.targetBuildDepends
    |> map toDependency
    |> (++ map toDependency constraints)
    |> toDependencies
  }


toCondition :: [Cabal.Condition Cabal.ConfVar] -> Condition
toCondition conditions = conditions
  |> combineConditions
  |> simplifyCondition
  |> renderCondition
  |> Text.pack
  |> Tagged.Tagged


combineConditions
  :: [Cabal.Condition Cabal.ConfVar] -> Cabal.Condition Cabal.ConfVar
combineConditions conditions = foldr Cabal.CAnd (Cabal.Lit True) conditions


simplifyCondition
  :: Cabal.Condition Cabal.ConfVar -> Cabal.Condition Cabal.ConfVar
simplifyCondition condition = case condition of
  Cabal.CNot this -> case simplifyCondition this of
    Cabal.Lit boolean -> Cabal.Lit (not boolean)
    Cabal.CNot that -> that
    that -> Cabal.CNot that
  Cabal.CAnd left right ->
    case (simplifyCondition left, simplifyCondition right) of
      (_, Cabal.Lit False) -> Cabal.Lit False
      (Cabal.Lit False, _) -> Cabal.Lit False
      (newLeft, Cabal.Lit True) -> newLeft
      (Cabal.Lit True, newRight) -> newRight
      (newLeft, newRight) -> Cabal.CAnd newLeft newRight
  Cabal.COr left right ->
    case (simplifyCondition left, simplifyCondition right) of
      (_, Cabal.Lit True) -> Cabal.Lit True
      (Cabal.Lit True, _) -> Cabal.Lit True
      (newLeft, Cabal.Lit False) -> newLeft
      (Cabal.Lit False, newRight) -> newRight
      (newLeft, newRight) -> Cabal.COr newLeft newRight
  _ -> condition


renderCondition :: Cabal.Condition Cabal.ConfVar -> String
renderCondition condition = case condition of
  Cabal.CAnd left right ->
    concat ["(", renderCondition left, " && ", renderCondition right, ")"]
  Cabal.CNot this -> concat ["!(", renderCondition this, ")"]
  Cabal.COr left right ->
    unwords ["(", renderCondition left, " || ", renderCondition right, ")"]
  Cabal.Lit boolean -> if boolean then "true" else "false"
  Cabal.Var confVar -> renderConfVar confVar


renderConfVar :: Cabal.ConfVar -> String
renderConfVar confVar = case confVar of
  Cabal.Arch arch -> concat ["arch(", Cabal.display arch, ")"]
  Cabal.Flag flag -> concat ["flag(", Cabal.unFlagName flag, ")"]
  Cabal.Impl compiler constraint -> concat
    [ "impl("
    , Cabal.display compiler
    , " "
    , constraint |> Cabal.simplifyVersionRange |> Cabal.display
    , ")"
    ]
  Cabal.OS os -> concat ["os(", Cabal.display os, ")"]


toDependencies :: [Dependency] -> Dependencies
toDependencies dependencies = dependencies
  |> map (\ dependency ->
    (dependencyPackage dependency, dependencyVersionRange dependency))
  |> Map.fromListWith Cabal.intersectVersionRanges
  |> Map.map (\ versionRange -> versionRange
    |> Cabal.simplifyVersionRange
    |> Cabal.display
    |> Text.pack
    |> Tagged.Tagged)


toDependency :: Cabal.Dependency -> Dependency
toDependency (Cabal.Dependency packageName versionRange) = Dependency
  { dependencyPackage = packageName
    |> Cabal.unPackageName
    |> Text.pack
    |> Tagged.Tagged
  , dependencyVersionRange = versionRange
  }


toExecutable
  :: (Cabal.Executable, ([Cabal.Condition Cabal.ConfVar], [Cabal.Dependency]))
  -> Executable
toExecutable (executable, (conditions, constraints)) = Executable
  { executableName = executable
    |> Cabal.exeName
    |> Cabal.unUnqualComponentName
    |> Text.pack
    |> Tagged.Tagged
  , executableCondition = toCondition conditions
  , executableDependencies = executable
    |> Cabal.buildInfo
    |> Cabal.targetBuildDepends
    |> map toDependency
    |> (++ map toDependency constraints)
    |> toDependencies
  }


toTest
  :: (Cabal.TestSuite, ([Cabal.Condition Cabal.ConfVar], [Cabal.Dependency]))
  -> Test
toTest (test, (conditions, constraints)) = Test
  { testName = test
    |> Cabal.testName
    |> Cabal.unUnqualComponentName
    |> Text.pack
    |> Tagged.Tagged
  , testCondition = toCondition conditions
  , testDependencies = test
    |> Cabal.testBuildInfo
    |> Cabal.targetBuildDepends
    |> map toDependency
    |> (++ map toDependency constraints)
    |> toDependencies
  }


toBenchmark
  :: (Cabal.Benchmark, ([Cabal.Condition Cabal.ConfVar], [Cabal.Dependency]))
  -> Benchmark
toBenchmark (benchmark, (conditions, constraints)) = Benchmark
  { benchmarkName = benchmark
    |> Cabal.benchmarkName
    |> Cabal.unUnqualComponentName
    |> Text.pack
    |> Tagged.Tagged
  , benchmarkCondition = toCondition conditions
  , benchmarkDependencies = benchmark
    |> Cabal.benchmarkBuildInfo
    |> Cabal.targetBuildDepends
    |> map toDependency
    |> (++ map toDependency constraints)
    |> toDependencies
  }


withoutConditionsOrConstraints
  :: a -> (a, ([Cabal.Condition Cabal.ConfVar], [Cabal.Dependency]))
withoutConditionsOrConstraints x = (x, ([], []))


nodeToTree
  :: Cabal.Condition v
  -> Cabal.CondTree v c a
  -> Tree.Tree (Cabal.Condition v, c, a)
nodeToTree condition node = Tree.Node
  { Tree.rootLabel =
    (condition, Cabal.condTreeConstraints node, Cabal.condTreeData node)
  , Tree.subForest = node
    |> Cabal.condTreeComponents
    |> concatMap branchToForest
  }


branchToForest
  :: Cabal.CondBranch v c a
  -> Tree.Forest (Cabal.Condition v, c, a)
branchToForest branch = let
  condition = Cabal.condBranchCondition branch
  ifTrue = Cabal.condBranchIfTrue branch
  maybeIfFalse = Cabal.condBranchIfFalse branch
  first = nodeToTree condition ifTrue
  rest = case maybeIfFalse of
    Nothing -> []
    Just ifFalse -> [nodeToTree (Cabal.CNot condition) ifFalse]
  in first : rest


fromCondTree :: Cabal.CondTree v [c] a -> [(a, ([Cabal.Condition v], [c]))]
fromCondTree tree = tree |> nodeToTree (Cabal.Lit True) |> foldTree [] []


foldTree
  :: [Cabal.Condition v]
  -> [c]
  -> Tree.Tree (Cabal.Condition v, [c], a)
  -> [(a, ([Cabal.Condition v], [c]))]
foldTree conditions constraints tree = case Tree.rootLabel tree of
  (condition, newConstraints, x) -> let
    allConditions = condition : conditions
    allConstraints = newConstraints ++ constraints
    first = (x, (allConditions, allConstraints))
    rest = tree
      |> Tree.subForest
      |> concatMap (foldTree allConditions allConstraints)
    in first : rest


fromEntries :: Tar.Entries a -> [Tar.Entry]
fromEntries = Tar.foldEntries (:) [] (\ _ -> [])


parseDescription :: Catch.MonadThrow m => LazyText.Text -> m Cabal.GenericPackageDescription
parseDescription contents = contents
  |> LazyText.unpack
  |> Cabal.parseGenericPackageDescription
  |> fromParseResult


fromParseResult :: Catch.MonadThrow m => Cabal.ParseResult a -> m a
fromParseResult result = case result of
  Cabal.ParseOk _ value -> pure value
  Cabal.ParseFailed problem ->
    "failed to parse package description: " ++ show problem |> throw


getEntryContents :: Catch.MonadThrow m => Tar.Entry -> m LazyBytes.ByteString
getEntryContents entry = case Tar.entryContent entry of
  Tar.NormalFile contents _ -> pure contents
  problem -> "failed to get entry contents: " ++ show problem
    |> throw


isCabal :: Tar.Entry -> Bool
isCabal entry = entry
  |> Tar.entryPath
  |> Path.takeExtension
  |> (== ".cabal")


getCached :: FilePath -> IO LazyBytes.ByteString -> IO LazyBytes.ByteString
getCached file miss = do
  let path = Path.combine cacheDirectory file
  Exception.catch (LazyBytes.readFile path) (handleCacheMiss miss path)


handleCacheMiss
  :: IO LazyBytes.ByteString
  -> FilePath
  -> Exception.IOException
  -> IO LazyBytes.ByteString
handleCacheMiss miss path exception = do
  IO.hPrint IO.stderr exception
  contents <- miss
  Directory.createDirectoryIfMissing True cacheDirectory
  LazyBytes.writeFile path contents
  pure contents


cacheDirectory :: FilePath
cacheDirectory = Path.combine "data" "cache"


getLatestIndex :: Client.Manager -> IO LazyBytes.ByteString
getLatestIndex manager = do
  request <- Client.parseRequest indexUrl
  response <- Client.httpLbs request manager
  response |> Client.responseBody |> pure


getLatestIndexMd5 :: Client.Manager -> IO String
getLatestIndexMd5 manager = do
  getRequest <- Client.parseRequest indexUrl
  let headRequest = getRequest { Client.method = Http.methodHead }
  response <- Client.httpNoBody headRequest manager
  getContentMd5 response


indexUrl :: String
indexUrl = "https://hackage.haskell.org/01-index.tar.gz"


getContentMd5 :: Catch.MonadThrow m => Client.Response body -> m String
getContentMd5 response = do
  base64 <- response
    |> Client.responseHeaders
    |> lookup Http.hContentMD5
    |> fromJust "could not find Content-MD5 header"
  md5 <- base64 |> Base64.decode |> fromRight
  base16 <- md5 |> Base16.encode |> decodeUtf8
  base16 |> Text.unpack |> pure


decodeUtf8 :: Catch.MonadThrow m => Bytes.ByteString -> m Text.Text
decodeUtf8 bytes = case Text.decodeUtf8' bytes of
  Right text -> pure text
  Left problem -> "failed to decode strict UTF-8: " ++ show problem
    |> throw


lazyDecodeUtf8 :: Catch.MonadThrow m => LazyBytes.ByteString -> m LazyText.Text
lazyDecodeUtf8 bytes = case LazyText.decodeUtf8' bytes of
  Right text -> pure text
  Left problem -> "failed to decode lazy UTF-8: " ++ show problem
    |> throw


fromJust :: Catch.MonadThrow m => String -> Maybe a -> m a
fromJust message value = maybe (throw message) pure value


throw :: Catch.MonadThrow m => String -> m a
throw message = message |> userError |> Catch.throwM


fromRight :: (Catch.MonadThrow m, Show left) => Either left right -> m right
fromRight e = case e of
  Right r -> pure r
  Left l -> show l |> throw


prepend :: a -> [a] -> [a]
prepend x xs = x : xs
