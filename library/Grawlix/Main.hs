{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}

module Grawlix.Main where

import Flow ((|>))
import Grawlix.Database
import Grawlix.Types

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as Gzip
import qualified Control.Arrow as Arrow
import qualified Control.Exception as Exception
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Foldable as Foldable
import qualified Data.Int as Int
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Tagged as Tagged
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Tree as Tree
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.Text as Cabal
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
  maybeMd5 <- getLatestIndexMd5 manager
  md5 <- maybe (fail "could not get package index MD5") pure maybeMd5
  let file = Path.addExtension md5 "tgz"
  index <- manager |> getLatestIndex |> getCached file
  index
    |> Gzip.decompress
    |> Tar.read
    |> fromEntries
    |> filter isCabal
    |> Maybe.mapMaybe getEntryContents
    |> Maybe.mapMaybe lazyDecodeUtf8
    |> Maybe.mapMaybe parseDescription
    |> map toPackage
    |> mapM_ (handlePackage connection)


handlePackage :: Sql.Connection -> Package -> IO ()
handlePackage connection package = do
  logPackage package

  let name = packageName package
  runQuery connection insertPackageName name

  let version = packageVersion package
  runQuery connection insertVersion version

  let license = packageLicense package
  runQuery connection insertLicense license

  let revision = packageRevision package
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
    libraryNameId <- runQuery connection selectLibraryName libraryName

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
        runQuery connection insertDependencyLibrary (dependencyId, libraryId)))

  -- TODO: More stuff.


logPackage :: Package -> IO ()
logPackage package = Printf.printf "%s\t%s\t%d\n"
  (package |> packageName |> Tagged.untag |> Text.unpack)
  (package
    |> packageVersion
    |> Tagged.untag
    |> map show
    |> List.intercalate ".")
  (package |> packageRevision |> Tagged.untag)


toPackage :: Cabal.GenericPackageDescription -> Package
toPackage package = Package
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
    |> Cabal.versionBranch
    |> map intToInt32
    |> Tagged.Tagged
  , packageRevision = package
    |> Cabal.packageDescription
    |> Cabal.customFieldsPD
    |> lookup "x-revision"
    |> Maybe.fromMaybe ""
    |> Read.readMaybe
    |> Maybe.fromMaybe 0
    |> intToInt32
    |> Tagged.Tagged
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
  , packageRepos = package
    |> Cabal.packageDescription
    |> Cabal.sourceRepos
    |> Maybe.mapMaybe toRepo
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


toRepo :: Cabal.SourceRepo -> Maybe Repo
toRepo repo = do
  let repoKind = repo
        |> Cabal.repoKind
        |> Cabal.display
        |> Text.pack
        |> Tagged.Tagged
  rawRepoType <- Cabal.repoType repo
  let repoType = rawRepoType
        |> Cabal.display
        |> Text.pack
        |> Tagged.Tagged
  rawRepoUrl <- Cabal.repoLocation repo
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
  Cabal.Flag (Cabal.FlagName flag) -> concat ["flag(", flag, ")"]
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
  { executableName = executable |> Cabal.exeName |> Text.pack |> Tagged.Tagged
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
  { testName = test |> Cabal.testName |> Text.pack |> Tagged.Tagged
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
    |> concatMap componentToForest
  }


componentToForest
  :: (Cabal.Condition v, Cabal.CondTree v c a, Maybe (Cabal.CondTree v c a))
  -> Tree.Forest (Cabal.Condition v, c, a)
componentToForest (condition, ifTrue, maybeIfFalse) = let
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


parseDescription :: LazyText.Text -> Maybe Cabal.GenericPackageDescription
parseDescription contents = contents
  |> LazyText.unpack
  |> Cabal.parsePackageDescription
  |> fromParseResult


fromParseResult :: Cabal.ParseResult a -> Maybe a
fromParseResult result = case result of
  Cabal.ParseOk _ value -> Just value
  _ -> Nothing


getEntryContents :: Tar.Entry -> Maybe LazyBytes.ByteString
getEntryContents entry = case Tar.entryContent entry of
  Tar.NormalFile contents _ -> Just contents
  _ -> Nothing


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
cacheDirectory = "cache"


getLatestIndex :: Client.Manager -> IO LazyBytes.ByteString
getLatestIndex manager = do
  request <- Client.parseRequest indexUrl
  response <- Client.httpLbs request manager
  response |> Client.responseBody |> pure


getLatestIndexMd5 :: Client.Manager -> IO (Maybe String)
getLatestIndexMd5 manager = do
  getRequest <- Client.parseRequest indexUrl
  let headRequest = getRequest { Client.method = Http.methodHead }
  response <- Client.httpNoBody headRequest manager
  response |> getContentMd5 |> pure


indexUrl :: String
indexUrl = "https://hackage.haskell.org/01-index.tar.gz"


getContentMd5 :: Client.Response body -> Maybe String
getContentMd5 response = do
  base64 <- response |> Client.responseHeaders |> lookup Http.hContentMD5
  md5 <- base64 |> Base64.decode |> fromEither
  base16 <- md5 |> Base16.encode |> decodeUtf8
  base16 |> Text.unpack |> pure


decodeUtf8 :: Bytes.ByteString -> Maybe Text.Text
decodeUtf8 contents = contents |> Text.decodeUtf8' |> fromEither


lazyDecodeUtf8 :: LazyBytes.ByteString -> Maybe LazyText.Text
lazyDecodeUtf8 contents = contents |> LazyText.decodeUtf8' |> fromEither


fromEither :: Either left right -> Maybe right
fromEither e = case e of
  Right r -> Just r
  _ -> Nothing


intToInt32 :: Int -> Int.Int32
intToInt32 = fromIntegral


prepend :: a -> [a] -> [a]
prepend x xs = x : xs
