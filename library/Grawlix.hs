{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PolyKinds #-}
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
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Tagged as Tagged
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
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


data Package = Package
  { packageName :: PackageName
  , packageVersion :: Version
  , packageRevision :: Revision
  , packageLicense :: License
  , packageSynopsis :: String
  , packageDescription :: String
  , packageCategories :: [Category]
  , packageUrl :: String
  , packageRepos :: [Repo]
  , packageLibraries :: [Library]
  , packageExecutables :: [Executable]
  , packageTests :: [Test]
  , packageBenchmarks :: [Benchmark]
  } deriving Show


type PackageName = Tagged.Tagged "PackageName" Text.Text


type Version = Tagged.Tagged "Version" [Int.Int32]


type Revision = Tagged.Tagged "Revision" Int.Int32


type License = Tagged.Tagged "License" Text.Text


type Category = Tagged.Tagged "Category" Text.Text


type PackageId = Tagged.Tagged "PackageId" Int.Int32


type CategoryId = Tagged.Tagged "CategoryId" Int.Int32


data Repo = Repo
  { repoType :: Cabal.RepoType
  , repoUrl :: String
  } deriving Show


data Library = Library
  { libraryModules :: [ModuleName]
  , libraryDependencies :: [Dependency]
  } deriving Show


type ModuleName = Tagged.Tagged "ModuleName" [Text.Text]


data Dependency = Dependency
  { dependencyPackage :: PackageName
  , dependencyConstraint :: Constraint
  } deriving Show


type Constraint = Tagged.Tagged "Constraint" Text.Text


data Executable = Executable
  { executableName :: String
  , executableDependencies :: [Dependency]
  } deriving Show


data Test = Test
  { testName :: String
  , testDependencies :: [Dependency]
  } deriving Show


data Benchmark = Benchmark
  { benchmarkName :: String
  , benchmarkDependencies :: [Dependency]
  } deriving Show


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
    , package |> packageSynopsis |> Text.pack
    , package |> packageDescription |> Text.pack
    , package |> packageUrl |> Text.pack
    )

  packageId <- runQuery connection selectPackageId (name, version, revision)

  package
    |> packageCategories
    |> mapM_ (\ category -> do
      runQuery connection insertCategory category
      categoryId <- runQuery connection selectCategoryId category
      runQuery connection insertCategoryPackage (categoryId, packageId))

  package
    |> packageLibraries
    |> mapM_ (\ library -> do
      -- TODO: Actually insert libraries.

      library
        |> libraryModules
        |> mapM_ (\ moduleName -> do
          runQuery connection insertModuleName moduleName
          {- TODO: Connect module names to libraries. -})

      library
        |> libraryDependencies
        |> mapM_ (\ dependency -> do
          dependency
            |> dependencyConstraint
            |> runQuery connection insertConstraint
          {- TODO: Actually insert dependencies. -}
          {- TODO: Connect dependencies to libraries. -}))

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


insertConstraint :: Sql.Query Constraint ()
insertConstraint = makeQuery
  [QQ.string|
    insert into constraints ( content )
    values ( $1 )
    on conflict do nothing
  |]
  (Sql.Encode.text |> Sql.Encode.value |> contraUntag)
  Sql.Decode.unit


insertModuleName :: Sql.Query ModuleName ()
insertModuleName = makeQuery
  [QQ.string|
    insert into module_names ( content )
    values ( $1 )
    on conflict do nothing
  |]
  (Sql.Encode.text |> arrayOf |> contraUntag)
  Sql.Decode.unit


insertPackageName :: Sql.Query PackageName ()
insertPackageName = makeQuery
  [QQ.string|
    insert into package_names ( content )
    values ( $1 )
    on conflict do nothing
  |]
  (Sql.Encode.text |> Sql.Encode.value |> contraUntag)
  Sql.Decode.unit


insertVersion :: Sql.Query Version ()
insertVersion = makeQuery
  [QQ.string|
    insert into versions ( content )
    values ( $1 )
    on conflict do nothing
  |]
  (Sql.Encode.int4 |> arrayOf |> contraUntag)
  Sql.Decode.unit


insertLicense :: Sql.Query License ()
insertLicense = makeQuery
  [QQ.string|
    insert into licenses ( content )
    values ( $1 )
    on conflict do nothing
  |]
  (Sql.Encode.text |> Sql.Encode.value |> contraUntag)
  Sql.Decode.unit


insertPackage
  :: Sql.Query
    ( PackageName
    , Version
    , Revision
    , License
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
    (Sql.Encode.text |> Sql.Encode.value |> contraUntag)
    (Sql.Encode.int4 |> arrayOf |> contraUntag)
    (Sql.Encode.int4 |> Sql.Encode.value |> contraUntag)
    (Sql.Encode.text |> Sql.Encode.value |> contraUntag)
    (Sql.Encode.value Sql.Encode.text)
    (Sql.Encode.value Sql.Encode.text)
    (Sql.Encode.value Sql.Encode.text))
  Sql.Decode.unit


selectPackageId :: Sql.Query (PackageName, Version, Revision) PackageId
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
    (Sql.Encode.text |> Sql.Encode.value |> contraUntag)
    (Sql.Encode.int4 |> arrayOf |> contraUntag)
    (Sql.Encode.int4 |> Sql.Encode.value |> contraUntag))
  (Sql.Decode.int4
    |> Sql.Decode.value
    |> Sql.Decode.singleRow
    |> fmap Tagged.Tagged)


insertCategory :: Sql.Query Category ()
insertCategory = makeQuery
  [QQ.string|
    insert into categories ( content )
    values ( $1 )
    on conflict do nothing
  |]
  (Sql.Encode.text |> Sql.Encode.value |> contraUntag)
  Sql.Decode.unit


selectCategoryId :: Sql.Query Category CategoryId
selectCategoryId = makeQuery
  [QQ.string|
    select id
    from categories
    where content = $1
  |]
  (Sql.Encode.text |> Sql.Encode.value |> contraUntag)
  (Sql.Decode.int4
    |> Sql.Decode.value
    |> Sql.Decode.singleRow
    |> fmap Tagged.Tagged)


insertCategoryPackage :: Sql.Query (CategoryId, PackageId) ()
insertCategoryPackage = makeQuery
  [QQ.string|
    insert into categories_packages ( category_id, package_id )
    values ( $1, $2 )
    on conflict do nothing
  |]
  (Contravariant.contrazip2
    (Sql.Encode.int4 |> Sql.Encode.value |> contraUntag)
    (Sql.Encode.int4 |> Sql.Encode.value |> contraUntag))
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
    |> Cabal.disp
    |> Pretty.render
    |> Text.pack
    |> Tagged.Tagged
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
    |> map Tagged.Tagged
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
  { libraryModules = library
    |> Cabal.exposedModules
    |> map (\ moduleName -> moduleName
      |> Cabal.components
      |> map Text.pack
      |> Tagged.Tagged)
  , libraryDependencies = library
    |> Cabal.libBuildInfo
    |> Cabal.targetBuildDepends
    |> map toDependency
  }


toDependency :: Cabal.Dependency -> Dependency
toDependency (Cabal.Dependency packageName versionRange) = Dependency
  { dependencyPackage = packageName
    |> Cabal.unPackageName
    |> Text.pack
    |> Tagged.Tagged
  , dependencyConstraint = versionRange
    |> Cabal.simplifyVersionRange
    |> Cabal.disp
    |> Pretty.render
    |> Text.pack
    |> Tagged.Tagged
  }


toExecutable :: Cabal.Executable -> Executable
toExecutable executable = Executable
  { executableName = executable |> Cabal.exeName
  , executableDependencies = executable
    |> Cabal.buildInfo
    |> Cabal.targetBuildDepends
    |> map toDependency
  }


toTest :: Cabal.TestSuite -> Test
toTest test = Test
  { testName = test |> Cabal.testName
  , testDependencies = test
    |> Cabal.testBuildInfo
    |> Cabal.targetBuildDepends
    |> map toDependency
  }


toBenchmark :: Cabal.Benchmark -> Benchmark
toBenchmark benchmark = Benchmark
  { benchmarkName = benchmark |> Cabal.benchmarkName
  , benchmarkDependencies = benchmark
    |> Cabal.benchmarkBuildInfo
    |> Cabal.targetBuildDepends
    |> map toDependency
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


contraUntag :: Contravariant.Contravariant f => f a -> f (Tagged.Tagged t a)
contraUntag = Contravariant.contramap Tagged.untag


runMigration :: Sql.Connection -> Sql.MigrationCommand -> IO ()
runMigration connection migration = do
  let session = migration
        |> Sql.runMigration
        |> Sql.transaction Sql.Transaction.Serializable Sql.Transaction.Write
  result <- Sql.run session connection
  case result of
    Right Sql.MigrationSuccess -> pure ()
    _ -> result |> show |> fail


prepend :: a -> [a] -> [a]
prepend x xs = x : xs


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
