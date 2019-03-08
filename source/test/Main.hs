{- HLint ignore "Redundant do" -}

module Main
  ( main
  )
where

import qualified Control.Monad.Trans.Maybe as MaybeT
import qualified Data.ByteString as ByteString
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Encoding
import qualified Data.Version as Version
import qualified Grawlix
import qualified Language.Haskell.Brittany as Brittany
import qualified Language.Haskell.HLint3 as Hlint
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import qualified Test.Hspec as Hspec

main :: IO ()
main = Hspec.hspec $ do

  Hspec.describe "Grawlix" $ do

    Hspec.describe "Version" $ do

      Hspec.describe "version" $ do

        Hspec.it "has four branches" $ do
          Version.versionBranch Grawlix.version
            `Hspec.shouldSatisfy` ((== 4) . length)

        Hspec.it "starts with one" $ do
          Version.versionBranch Grawlix.version
            `Hspec.shouldSatisfy` List.isPrefixOf [1]

        Hspec.it "has no tags" $ do
          versionTags Grawlix.version `Hspec.shouldSatisfy` null

      Hspec.describe "versionString" $ do

        Hspec.it "uses Data.Version.showVersion" $ do
          Grawlix.versionString
            `Hspec.shouldBe` Version.showVersion Grawlix.version

  Hspec.describe "hlint" $ do

    Hspec.it "source" $ do
      ideas <- Hlint.hlint ["lint", "--quiet", "source"]
      ideas `Hspec.shouldSatisfy` null

  Hspec.describe "brittany" $ do
    config <- Hspec.runIO getBrittanyConfig
    files <- Hspec.runIO getHaskellFiles
    mapM_ (checkFormatting config) files

versionTags :: Version.Version -> [String]
versionTags (Version.Version _ tags) = tags

getBrittanyConfig :: IO Brittany.Config
getBrittanyConfig = do
  maybeFile <- Brittany.findLocalConfigPath "."
  fmap (Maybe.fromMaybe Brittany.staticDefaultConfig)
    . MaybeT.runMaybeT
    . Brittany.readConfigsWithUserConfig mempty
    $ Maybe.maybeToList maybeFile

getHaskellFiles :: IO [FilePath]
getHaskellFiles =
  filter (FilePath.isExtensionOf "hs") <$> listDirectoryRecursively "source"

listDirectoryRecursively :: FilePath -> IO [FilePath]
listDirectoryRecursively directory = do
  entries <- Directory.listDirectory directory
  concat <$> mapM (listEntryRecursively . FilePath.combine directory) entries

listEntryRecursively :: FilePath -> IO [FilePath]
listEntryRecursively entry = do
  isDirectory <- Directory.doesDirectoryExist entry
  if isDirectory then listDirectoryRecursively entry else pure [entry]

checkFormatting :: Brittany.Config -> FilePath -> Hspec.Spec
checkFormatting config file = Hspec.it file $ do
  byteString <- ByteString.readFile file
  actual <- either (fail . show) pure $ Encoding.decodeUtf8' byteString
  result <- Brittany.parsePrintModule config actual
  expected <- either (fail . unlines . fmap showBrittanyError) pure result
  normalizeNewlines actual `Hspec.shouldBe` expected

showBrittanyError :: Brittany.BrittanyError -> String
showBrittanyError brittanyError = case brittanyError of
  Brittany.ErrorInput x -> "ErrorInput " <> show x
  Brittany.ErrorMacroConfig x y ->
    "ErrorMacroConfig " <> show x <> " " <> show y
  Brittany.ErrorOutputCheck -> "ErrorOutputCheck"
  Brittany.ErrorUnknownNode x _ ->
    "ErrorUnknownNode " <> show x <> " undefined"
  Brittany.ErrorUnusedComment x -> "ErrorUnusedComment " <> show x
  Brittany.LayoutWarning x -> "LayoutWarning " <> show x

normalizeNewlines :: Text.Text -> Text.Text
normalizeNewlines = Text.replace (Text.pack "\r\n") $ Text.singleton '\n'
