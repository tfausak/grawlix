module Grawlix.Main
  ( defaultMain
  )
where

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.Zlib.Internal as Zlib
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as Stm
import qualified Control.Monad as Monad
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.Parsec.Common as Cabal
import qualified Distribution.Pretty as Cabal
import qualified Distribution.Types.GenericPackageDescription as Cabal
import qualified Distribution.Types.PackageDescription as Cabal
import qualified Distribution.Types.PackageId as Cabal
import qualified Distribution.Types.PackageName as Cabal
import qualified Grawlix.Config as Config
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Tls
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Http
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.IO as IO

defaultMain :: IO ()
defaultMain = do
  manager <- Tls.newTlsManager
  entityTagVar <- Stm.newTVarIO Nothing
  program <- Environment.getProgName
  arguments <- Environment.getArgs
  config <- case Config.getConfig program arguments of
    Left errors -> do
      IO.hPutStr IO.stderr errors
      Exit.exitFailure
    Right (config, warnings) -> do
      IO.hPutStr IO.stderr warnings
      pure config

  Sql.withConnection (Config.configDatabase config) $ \connection ->
    Monad.forever $ do
      Sql.execute_ connection
        $ query
            " create table if not exists packages \
            \ ( id integer primary key \
            \ , name text not null \
            \ , version text not null \
            \ , revision integer not null \
            \ , unique ( name, version, revision ) \
            \ ) "
      IO.hPutStrLn IO.stderr "Getting package index ..."
      initialRequest <- Client.parseRequest
        "https://hackage.haskell.org/01-index.tar.gz"
      maybeEntityTag <- Stm.readTVarIO entityTagVar
      let
        request = case maybeEntityTag of
          Nothing -> initialRequest
          Just entityTag -> initialRequest
            { Client.requestHeaders =
              [(Http.hIfNoneMatch, unwrapEntityTag entityTag)]
            }
      response <- Client.httpLbs request manager
      case Http.statusCode $ Client.responseStatus response of
        304 -> IO.hPutStrLn IO.stderr "Package index has not changed."
        200 -> do
          IO.hPutStrLn IO.stderr "Processing new package index ..."
          case
              fmap EntityTag . lookup Http.hETag $ Client.responseHeaders
                response
            of
              Nothing -> pure ()
              Just entityTag ->
                Stm.atomically . Stm.writeTVar entityTagVar $ Just entityTag
          case decompress $ Client.responseBody response of
            Left decompressError -> IO.hPrint IO.stderr decompressError
            Right archive ->
              mapM_ (processPackage connection)
                . fmap (parsePackage . toIndexEntry)
                . Tar.foldEntries (prepend . Right) [] (singleton . Left)
                $ Tar.read archive
          IO.hPutStrLn IO.stderr "Done processing package index."
        _ -> IO.hPrint IO.stderr response
      Concurrent.threadDelay $ 60 * 1000000

query :: String -> Sql.Query
query = Sql.Query . Text.pack

newtype EntityTag = EntityTag
  { unwrapEntityTag :: ByteString.ByteString
  } deriving (Eq, Show)

decompress
  :: LazyByteString.ByteString
  -> Either Zlib.DecompressError LazyByteString.ByteString
decompress = Zlib.safeDecompress Zlib.gzipFormat Zlib.defaultDecompressParams

singleton :: a -> [a]
singleton = pure

prepend :: a -> [a] -> [a]
prepend = (:)

toIndexEntry
  :: Either Tar.FormatError Tar.Entry -> Either IndexError IndexEntry
toIndexEntry result = case result of
  Left formatError -> Left $ IndexErrorInvalidArchive formatError
  Right entry -> case Tar.entryContent entry of
    Tar.BlockDevice{} -> Left $ IndexErrorBadEntryType entry
    Tar.CharacterDevice{} -> Left $ IndexErrorBadEntryType entry
    Tar.Directory{} -> Left $ IndexErrorBadEntryType entry
    Tar.HardLink{} -> Left $ IndexErrorBadEntryType entry
    Tar.NamedPipe{} -> Left $ IndexErrorBadEntryType entry
    Tar.NormalFile contents _ ->
      let filePath = Tar.entryPath entry
      in
        if FilePath.isExtensionOf "cabal" filePath
          then Right IndexEntry
            { indexEntryFilePath = filePath
            , indexEntryContents = contents
            }
          else Left $ IndexErrorBadExtension filePath
    Tar.OtherEntryType{} -> Left $ IndexErrorBadEntryType entry
    Tar.SymbolicLink{} -> Left $ IndexErrorBadEntryType entry

data IndexError
  = IndexErrorInvalidArchive Tar.FormatError
  | IndexErrorBadEntryType Tar.Entry
  | IndexErrorBadExtension FilePath
  deriving (Eq, Show)

data IndexEntry = IndexEntry
  { indexEntryFilePath :: FilePath
  , indexEntryContents :: LazyByteString.ByteString
  } deriving (Eq, Show)

parsePackage
  :: Either IndexError IndexEntry
  -> Either ParseError Cabal.GenericPackageDescription
parsePackage result = case result of
  Left indexError -> Left $ ParseErrorInvalidIndex indexError
  Right indexEntry ->
    mapLeft (ParseErrorInvalidPackage $ indexEntryFilePath indexEntry)
      . parseGenericPackageDescription
      $ indexEntryContents indexEntry

data ParseError
  = ParseErrorInvalidIndex IndexError
  | ParseErrorInvalidPackage FilePath [Cabal.PError]
  deriving (Show)

instance Eq ParseError where
  x == y = case (x, y) of
    (ParseErrorInvalidIndex x1, ParseErrorInvalidIndex y1) -> x1 == y1
    (ParseErrorInvalidPackage x1 x2, ParseErrorInvalidPackage y1 y2) ->
      x1 == y1 && eqPErrors x2 y2
    _ -> False

eqPErrors :: [Cabal.PError] -> [Cabal.PError] -> Bool
eqPErrors xs ys = case (xs, ys) of
  ([], []) -> True
  (xh : xt, yh : yt) -> eqPError xh yh && eqPErrors xt yt
  _ -> False

eqPError :: Cabal.PError -> Cabal.PError -> Bool
eqPError (Cabal.PError x1 x2) (Cabal.PError y1 y2) = x1 == y1 && x2 == y2

parseGenericPackageDescription
  :: LazyByteString.ByteString
  -> Either [Cabal.PError] Cabal.GenericPackageDescription
parseGenericPackageDescription =
  mapLeft snd
    . snd
    . Cabal.runParseResult
    . Cabal.parseGenericPackageDescription
    . LazyByteString.toStrict

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

processPackage
  :: Sql.Connection
  -> Either ParseError Cabal.GenericPackageDescription
  -> IO ()
processPackage connection result = case result of
  Left parseError -> case parseError of
    ParseErrorInvalidIndex (IndexErrorBadExtension _) -> pure ()
    _ -> IO.hPrint IO.stderr parseError
  Right package -> do
    let
      name =
        Cabal.unPackageName
          . Cabal.pkgName
          . Cabal.package
          $ Cabal.packageDescription package
      version =
        Cabal.prettyShow
          . Cabal.pkgVersion
          . Cabal.package
          $ Cabal.packageDescription package
      revision =
        Maybe.fromMaybe "0"
          . lookup "x-revision"
          . Cabal.customFieldsPD
          $ Cabal.packageDescription package
    Sql.executeNamed
      connection
      (query
        " insert or ignore into packages (name, version, revision) \
        \ values (:name, :version, :revision) "
      )
      [param "name" name, param "version" version, param "revision" revision]

param :: Sql.ToField v => String -> v -> Sql.NamedParam
param k v = Text.pack (':' : k) Sql.:= v
