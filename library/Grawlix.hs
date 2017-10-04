{-# LANGUAGE NamedFieldPuns #-}

module Grawlix (main) where

import Flow ((|>))

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as Gzip
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Distribution.License as Cabal
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.Version as Cabal
import qualified Hasql.Connection as Hasql
import qualified Hasql.Migration as Hasql
import qualified Hasql.Session as Hasql
import qualified Hasql.Transaction as Hasql
import qualified Hasql.Transaction.Sessions as Hasql
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Network.HTTP.Types as Http
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as Path


main :: IO ()
main = do
  connection <- do
    maybeSettings <- Environment.lookupEnv "DATABASE"
    result <- maybeSettings
      |> Maybe.fromMaybe ""
      |> Text.pack
      |> Text.encodeUtf8
      |> Hasql.acquire
    case result of
      Left problem -> fail (show problem)
      Right connection -> pure connection

  migrations <- Hasql.loadMigrationsFromDirectory "migrations"
  Monad.forM_ (Hasql.MigrationInitialization : migrations) (\ migration -> do
    let session = migration
          |> Hasql.runMigration
          |> Hasql.transaction Hasql.Serializable Hasql.Write
    result <- Hasql.run session connection
    case result of
      Right Hasql.MigrationSuccess -> pure ()
      _ -> fail (show result))

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
    -- TODO: Insert packages into database.
    |> take 1
    |> print


data Package = Package
  { packageName :: Cabal.PackageName
  , packageVersion :: Cabal.Version
  , packageLicense :: Cabal.License
  , packageSynopsis :: String
  , packageDescription :: String
  , packageCategory :: String
  , packageUrl :: String
  , packageRepos :: [Repo]
  , packageLibraries :: [Library]
  , packageExecutables :: [Executable]
  , packageTests :: [Test]
  , packageBenchmarks :: [Benchmark]
  } deriving Show


data Repo = Repo
  { repoType :: Cabal.RepoType
  , repoUrl :: String
  } deriving Show


data Library = Library
  { libraryModules :: [Cabal.ModuleName]
  , libraryDependencies :: [Cabal.Dependency]
  } deriving Show


data Executable = Executable
  { executableName :: String
  , executableDependencies :: [Cabal.Dependency]
  } deriving Show


data Test = Test
  { testName :: String
  , testDependencies :: [Cabal.Dependency]
  } deriving Show


data Benchmark = Benchmark
  { benchmarkName :: String
  , benchmarkDependencies :: [Cabal.Dependency]
  } deriving Show


toPackage :: Cabal.GenericPackageDescription -> Package
toPackage package = Package
  { packageName = package
    |> Cabal.packageDescription
    |> Cabal.package
    |> Cabal.pkgName
  , packageVersion = package
    |> Cabal.packageDescription
    |> Cabal.package
    |> Cabal.pkgVersion
  , packageLicense = package |> Cabal.packageDescription |> Cabal.license
  , packageSynopsis = package |> Cabal.packageDescription |> Cabal.synopsis
  , packageDescription = package
    |> Cabal.packageDescription
    |> Cabal.description
  , packageCategory = package |> Cabal.packageDescription |> Cabal.category
  , packageUrl = package |> Cabal.packageDescription |> Cabal.homepage
  , packageRepos = package
    |> Cabal.packageDescription
    |> Cabal.sourceRepos
    |> Maybe.mapMaybe toRepo
  , packageLibraries = let
    library = package
      |> Cabal.packageDescription
      |> Cabal.library
      |> Foldable.toList
    condLibrary = package
      |> Cabal.condLibrary
      |> Foldable.toList
      |> concatMap fromCondTree
    in [library, condLibrary] |> concat |> map toLibrary
  , packageExecutables = let
    executables = package |> Cabal.packageDescription |> Cabal.executables
    condExecutables = package
      |> Cabal.condExecutables
      |> concatMap (\ (name, tree) -> tree
        |> fromCondTree
        |> map (\ executable -> executable { Cabal.exeName = name }))
    in [executables, condExecutables] |> concat |> map toExecutable
  , packageTests = let
    tests = package |> Cabal.packageDescription |> Cabal.testSuites
    condTests = package
      |> Cabal.condTestSuites
      |> concatMap (\ (name, tree) -> tree
        |> fromCondTree
        |> map (\ test -> test { Cabal.testName = name }))
    in [tests, condTests] |> concat |> map toTest
  , packageBenchmarks = let
    benchmarks = package |> Cabal.packageDescription |> Cabal.benchmarks
    condBenchmarks = package
      |> Cabal.condBenchmarks
      |> concatMap (\ (name, tree) -> tree
        |> fromCondTree
        |> map (\ benchmark -> benchmark { Cabal.benchmarkName = name }))
    in [benchmarks, condBenchmarks] |> concat |> map toBenchmark
  }


toRepo :: Cabal.SourceRepo -> Maybe Repo
toRepo repo = do
  Monad.guard (Cabal.repoKind repo == Cabal.RepoHead)
  repoType <- Cabal.repoType repo
  repoUrl <- Cabal.repoLocation repo
  pure Repo { repoType, repoUrl }


toLibrary :: Cabal.Library -> Library
toLibrary library = Library
  { libraryModules = library |> Cabal.exposedModules
  , libraryDependencies = library
    |> Cabal.libBuildInfo
    |> Cabal.targetBuildDepends
  }


toExecutable :: Cabal.Executable -> Executable
toExecutable executable = Executable
  { executableName = executable |> Cabal.exeName
  , executableDependencies = executable
    |> Cabal.buildInfo
    |> Cabal.targetBuildDepends
  }


toTest :: Cabal.TestSuite -> Test
toTest test = Test
  { testName = test |> Cabal.testName
  , testDependencies = test |> Cabal.testBuildInfo |> Cabal.targetBuildDepends
  }


toBenchmark :: Cabal.Benchmark -> Benchmark
toBenchmark benchmark = Benchmark
  { benchmarkName = benchmark |> Cabal.benchmarkName
  , benchmarkDependencies = benchmark
    |> Cabal.benchmarkBuildInfo
    |> Cabal.targetBuildDepends
  }


fromCondTree :: Cabal.CondTree v c a -> [a]
fromCondTree tree = let
  root = Cabal.condTreeData tree
  leaves = Cabal.condTreeComponents tree
  branches = concatMap fromCondBranch leaves
  in root : concatMap fromCondTree branches


fromCondBranch
  :: (Cabal.Condition v, Cabal.CondTree v c a, Maybe (Cabal.CondTree v c a))
  -> [Cabal.CondTree v c a]
fromCondBranch (_, ifTrue, ifFalse) = ifTrue : Foldable.toList ifFalse


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
  print exception
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
