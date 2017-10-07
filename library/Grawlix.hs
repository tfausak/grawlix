{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Grawlix (main) where

import Flow ((|>))

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as Gzip
import qualified Contravariant.Extras as Contravariant
import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Data.ByteString as Bytes
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Foldable as Foldable
import qualified Data.Functor.Contravariant as Contravariant
import qualified Data.Int as Int
import qualified Data.Maybe as Maybe
import qualified Data.Tagged as Tagged
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Distribution.License as Cabal
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.Text as Cabal
import qualified Distribution.Version as Cabal
import qualified Hasql.Connection as Sql
import qualified Hasql.Decoders as Sql.Decode
import qualified Hasql.Encoders as Sql.Encode
import qualified Hasql.Migration as Sql
import qualified Hasql.Query as Sql
import qualified Hasql.Session as Sql
import qualified Hasql.Transaction as Sql.Transaction
import qualified Hasql.Transaction.Sessions as Sql
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client
import qualified Network.HTTP.Types as Http
import qualified QQ
import qualified System.Directory as Directory
import qualified System.Environment as Environment
import qualified System.FilePath as Path
import qualified Text.PrettyPrint as Pretty
import qualified Text.Printf as Printf
import qualified Text.Read as Read


main :: IO ()
main = do
  connection <- do
    maybeSettings <- Environment.lookupEnv "DATABASE"
    result <- maybeSettings
      |> Maybe.fromMaybe ""
      |> Text.pack
      |> Text.encodeUtf8
      |> Sql.acquire
    case result of
      Left problem -> problem |> show |> fail
      Right connection -> pure connection

  migrations <- Sql.loadMigrationsFromDirectory "migrations"
  Monad.forM_ (Sql.MigrationInitialization : migrations) (\ migration -> do
    let session = migration
          |> Sql.runMigration
          |> Sql.transaction Sql.Transaction.Serializable Sql.Transaction.Write
    result <- Sql.run session connection
    case result of
      Right Sql.MigrationSuccess -> pure ()
      _ -> result |> show |> fail)

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


data Package = Package
  { packageName :: Cabal.PackageName
  , packageVersion :: Cabal.Version
  , packageRevision :: Revision
  , packageLicense :: Cabal.License
  , packageSynopsis :: String
  , packageDescription :: String
  , packageCategories :: [Text.Text]
  , packageUrl :: String
  , packageRepos :: [Repo]
  , packageLibraries :: [Library]
  , packageExecutables :: [Executable]
  , packageTests :: [Test]
  , packageBenchmarks :: [Benchmark]
  } deriving Show


type Revision = Tagged.Tagged "Revision" Int.Int32


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


handlePackage :: Sql.Connection -> Package -> IO ()
handlePackage connection package = do
  Printf.printf "%s\t%s\t%d\n"
    (package |> packageName |> Cabal.unPackageName)
    (package |> packageVersion |> Cabal.disp |> Pretty.render)
    (package |> packageRevision |> Tagged.untag)

  let name = package |> packageName |> Cabal.unPackageName |> Text.pack
  runQuery connection insertPackageName name

  let version = package
        |> packageVersion
        |> Cabal.versionBranch
        |> map intToInt32
  runQuery connection insertVersion version

  let license = package
        |> packageLicense
        |> Cabal.disp
        |> Pretty.render
        |> Text.pack
  runQuery connection insertLicense license

  let revision = packageRevision package
  runQuery connection insertPackage
    ( name
    , version
    , revision
    , license
    , package |> packageSynopsis |> Text.pack
    , package |> packageDescription |> Text.pack
    , package |> packageUrl |> Text.pack
    )

  packageId <- runQuery connection selectPackageId (name, version, revision)

  let categories = package |> packageCategories
  Monad.forM_ categories (\ category -> do
    runQuery connection insertCategory category
    categoryId <- runQuery connection selectCategoryId category
    runQuery connection insertCategoryPackage (categoryId, packageId))

  -- TODO: More stuff.


insertPackageName :: Sql.Query Text.Text ()
insertPackageName = makeQuery
  [QQ.string|
    insert into package_names ( content )
    values ( $1 )
    on conflict do nothing
  |]
  (Sql.Encode.value Sql.Encode.text)
  Sql.Decode.unit


insertVersion :: Sql.Query [Int.Int32] ()
insertVersion = makeQuery
  [QQ.string|
    insert into versions ( content )
    values ( $1 )
    on conflict do nothing
  |]
  (arrayOf Sql.Encode.int4)
  Sql.Decode.unit


insertLicense :: Sql.Query Text.Text ()
insertLicense = makeQuery
  [QQ.string|
    insert into licenses ( content )
    values ( $1 )
    on conflict do nothing
  |]
  (Sql.Encode.value Sql.Encode.text)
  Sql.Decode.unit


insertPackage
  :: Sql.Query
    ( Text.Text
    , [Int.Int32]
    , Revision
    , Text.Text
    , Text.Text
    , Text.Text
    , Text.Text
    )
    ()
insertPackage = makeQuery
  [QQ.string|
    insert into packages (
      package_name_id,
      version_id,
      revision,
      license_id,
      synopsis,
      description,
      url
    ) values (
      ( select id from package_names where content = $1 ),
      ( select id from versions where content = $2 ),
      $3,
      ( select id from licenses where content = $4 ),
      $5,
      $6,
      $7
    ) on conflict do nothing
  |]
  (Contravariant.contrazip7
    (Sql.Encode.value Sql.Encode.text)
    (arrayOf Sql.Encode.int4)
    (Sql.Encode.int4
      |> Sql.Encode.value
      |> Contravariant.contramap Tagged.untag)
    (Sql.Encode.value Sql.Encode.text)
    (Sql.Encode.value Sql.Encode.text)
    (Sql.Encode.value Sql.Encode.text)
    (Sql.Encode.value Sql.Encode.text))
  Sql.Decode.unit


selectPackageId :: Sql.Query (Text.Text, [Int.Int32], Revision) Int.Int32
selectPackageId = makeQuery
  [QQ.string|
    select packages.id
    from packages
    inner join package_names
    on package_names.id = packages.package_name_id
    inner join versions
    on versions.id = packages.version_id
    where package_names.content = $1
    and versions.content = $2
    and packages.revision = $3
  |]
  (Contravariant.contrazip3
    (Sql.Encode.value Sql.Encode.text)
    (arrayOf Sql.Encode.int4)
    (Sql.Encode.int4
      |> Sql.Encode.value
      |> Contravariant.contramap Tagged.untag))
  (Sql.Decode.int4 |> Sql.Decode.value |> Sql.Decode.singleRow)


insertCategory :: Sql.Query Text.Text ()
insertCategory = makeQuery
  [QQ.string|
    insert into categories ( content )
    values ( $1 )
    on conflict do nothing
  |]
  (Sql.Encode.value Sql.Encode.text)
  Sql.Decode.unit


selectCategoryId :: Sql.Query Text.Text Int.Int32
selectCategoryId = makeQuery
  [QQ.string|
    select id
    from categories
    where content = $1
  |]
  (Sql.Encode.value Sql.Encode.text)
  (Sql.Decode.int4 |> Sql.Decode.value |> Sql.Decode.singleRow)


insertCategoryPackage :: Sql.Query (Int.Int32, Int.Int32) ()
insertCategoryPackage = makeQuery
  [QQ.string|
    insert into categories_packages ( category_id, package_id )
    values ( $1, $2 )
    on conflict do nothing
  |]
  (Contravariant.contrazip2
    (Sql.Encode.value Sql.Encode.int4)
    (Sql.Encode.value Sql.Encode.int4))
  Sql.Decode.unit


makeQuery
  :: String -> Sql.Encode.Params a -> Sql.Decode.Result b -> Sql.Query a b
makeQuery rawSql encoder decoder =
  let
    sql = rawSql |> Text.pack |> Text.encodeUtf8
    prepare = True
  in Sql.statement sql encoder decoder prepare


runQuery :: Sql.Connection -> Sql.Query a b -> a -> IO b
runQuery connection query params = do
  let session = Sql.query params query
  result <- Sql.run session connection
  case result of
    Left problem -> fail (show problem)
    Right value -> pure value


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
  , packageRevision = package
    |> Cabal.packageDescription
    |> Cabal.customFieldsPD
    |> lookup "x-revision"
    |> Maybe.fromMaybe ""
    |> Read.readMaybe
    |> Maybe.fromMaybe 0
    |> intToInt32
    |> Tagged.Tagged
  , packageLicense = package |> Cabal.packageDescription |> Cabal.license
  , packageSynopsis = package |> Cabal.packageDescription |> Cabal.synopsis
  , packageDescription = package
    |> Cabal.packageDescription
    |> Cabal.description
  , packageCategories = package
    |> Cabal.packageDescription
    |> Cabal.category
    |> Text.pack
    |> Text.splitOn (Text.singleton ',')
    |> map Text.strip
    |> filter (\ category -> category |> Text.null |> not)
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


intToInt32 :: Int -> Int.Int32
intToInt32 = fromIntegral


arrayOf :: Sql.Encode.Value a -> Sql.Encode.Params [a]
arrayOf x = x
  |> Sql.Encode.arrayValue
  |> Sql.Encode.arrayDimension foldl
  |> Sql.Encode.array
  |> Sql.Encode.value
