import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as Gzip
import qualified Control.DeepSeq as DeepSeq
import qualified Control.Exception as Exception
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Time as Time
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.GenericPackageDescription as Cabal
import qualified Distribution.Types.PackageDescription as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Distribution.Types.Version as Cabal
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls
import qualified Numeric.Natural as Natural
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath

main :: IO ()
main = do

  logLn ["Getting package index ..."]
  compressed <- getIndex
  logLn ["Got package index."]

  logLn
    [ "Decompressing"
    , show (LazyByteString.length (unwrapCompressed compressed))
    , "byte package index ..."
    ]
  archive <- decompress compressed
  logLn ["Decompressed package index."]

  logLn
    [ "Unpacking"
    , show (LazyByteString.length (unwrapArchive archive))
    , "byte package index ..."
    ]
  entries <- unpack archive
  logLn ["Unpacked package descriptions."]

  logLn ["Parsing", show (length entries), "package descriptions ..."]
  descriptions <- traverse parse entries
  logLn ["Parsed package descriptions."]

  logLn ["Converting", show (length descriptions), "packages ..."]
  packages <- convert descriptions
  logLn ["Converted packages."]

  -- TODO: Get the exposed modules out of all the package library components.
  mapM_ (logLn . (: []) . render) packages

logLn :: [String] -> IO ()
logLn messages = do
  now <- Time.getCurrentTime
  putStrLn (unwords
    ( Time.formatTime Time.defaultTimeLocale "%Y-%m-%dT%H:%M:%S%3QZ" now
    : messages
    ))

newtype Compressed = Compressed
  { unwrapCompressed :: LazyByteString.ByteString
  } deriving (Eq, Show)

getIndex :: IO Compressed
getIndex = do
  exists <- Directory.doesFileExist indexFile
  if exists then readIndex else do
    index <- fetchIndex
    writeIndex index
    pure index

indexFile :: FilePath
indexFile = "hackage.tgz"

fetchIndex :: IO Compressed
fetchIndex = do
  request <- Client.parseUrlThrow "https://hackage.haskell.org/01-index.tar.gz"
  manager <- Tls.newTlsManager
  response <- Client.httpLbs request manager
  pure (Compressed (Client.responseBody response))

writeIndex :: Compressed -> IO ()
writeIndex = LazyByteString.writeFile indexFile . unwrapCompressed

readIndex :: IO Compressed
readIndex = fmap Compressed (LazyByteString.readFile indexFile)

newtype Archive = Archive
  { unwrapArchive :: LazyByteString.ByteString
  } deriving (Eq, Show)

decompress :: Compressed -> IO Archive
decompress
  = fmap Archive
  . Exception.evaluate
  . DeepSeq.force
  . Gzip.decompress
  . unwrapCompressed

newtype Entry = Entry
  { unwrapEntry :: LazyByteString.ByteString
  } deriving (Eq, Show)

unpack :: Archive -> IO [Entry]
unpack
  = Tar.foldEntries handleEntry (pure []) Exception.throwIO
  . Tar.read
  . unwrapArchive

handleEntry :: Tar.Entry -> IO [Entry] -> IO [Entry]
handleEntry entry action = do
  entries <- action
  pure (case Tar.entryContent entry of
    Tar.NormalFile contents _ ->
      if FilePath.isExtensionOf "cabal" (Tar.entryPath entry)
        then Entry contents : entries
        else entries
    _ -> entries)

parse :: Entry -> IO Cabal.GenericPackageDescription
parse
  = either (fail . show . snd) pure
  . snd
  . Cabal.runParseResult
  . Cabal.parseGenericPackageDescription
  . LazyByteString.toStrict
  . unwrapEntry

data Package = Package
  { packageIdentifier :: Identifier
  , packageRevision :: Natural.Natural
  } deriving (Eq, Show)

convert :: [Cabal.GenericPackageDescription] -> IO [Package]
convert = fmap fst . foldr
  (\ gpd action -> do
    (packages, revisions) <- action
    let
      identifier = identify gpd
      (revision, newRevisions) = case Map.lookup identifier revisions of
        Nothing -> (0, Map.insert identifier 0 revisions)
        Just rev -> (rev, Map.adjust (+ 1) identifier revisions)
      package = Package
        { packageIdentifier = identifier
        , packageRevision = revision
        }
    pure (package : packages, newRevisions))
  (pure ([], Map.empty))

data Identifier = Identifier
  { identifierName :: Cabal.PackageName
  , identifierVersion :: Cabal.Version
  } deriving (Eq, Ord, Show)

identify :: Cabal.GenericPackageDescription -> Identifier
identify gpd = Identifier
  { identifierName = Cabal.pkgName (Cabal.package (Cabal.packageDescription gpd))
  , identifierVersion = Cabal.pkgVersion (Cabal.package (Cabal.packageDescription gpd))
  }

render :: Package -> String
render package = List.intercalate "\t"
  [ show (packageRevision package)
  , Cabal.prettyShow (identifierVersion (packageIdentifier package))
  , Cabal.unPackageName (identifierName (packageIdentifier package))
  ]
