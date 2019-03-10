module Grawlix.Config
  ( Config(..)
  , defaultConfig
  , getConfig
  )
where

import qualified Data.List as List
import qualified Grawlix.Version as Version
import qualified System.Console.GetOpt as Console

data Config = Config
  { configDatabase :: String
  , configHelp :: Bool
  , configVersion :: Bool
  } deriving (Eq, Show)

defaultConfig :: Config
defaultConfig = Config
  { configDatabase = ":memory:"
  , configHelp = False
  , configVersion = False
  }

getConfig :: String -> [String] -> Either String (Config, String)
getConfig program arguments =
  let
    (updates, unexpectedArguments, unknownOptions, errors) =
      Console.getOpt' Console.Permute options arguments
    warnings =
      unlines
        . fmap (mappend "WARNING: ")
        $ fmap unknownOptionWarning unknownOptions
        <> fmap unexpectedArgumentWarning unexpectedArguments
  in case (errors, applyUpdates updates) of
    (_ : _, _) -> Left $ concatMap (mappend "ERROR: ") errors
    ([], Left problem) -> Left . withNewline $ mappend "ERROR: " problem
    ([], Right config)
      | configHelp config -> Left $ Console.usageInfo program options
      | configVersion config -> Left $ withNewline Version.versionString
      | otherwise -> Right (config, warnings)

type Option = Console.OptDescr Update

type Argument = Console.ArgDescr Update

type Update = Config -> Either String Config

options :: [Option]
options = [databaseOption, helpOption, versionOption]

databaseOption :: Option
databaseOption =
  makeOption ['d'] ["database"] "SQLite database to connect to"
    . requiredArgument "FILE"
    $ \database config -> Right config { configDatabase = database }

helpOption :: Option
helpOption =
  makeOption ['h', '?'] ["help"] "show the help" . Console.NoArg $ \config ->
    Right config { configHelp = True }

versionOption :: Option
versionOption =
  makeOption ['v'] ["version"] "show the version" . Console.NoArg $ \config ->
    Right config { configVersion = True }

makeOption :: String -> [String] -> String -> Argument -> Option
makeOption short long help argument = Console.Option short long argument help

requiredArgument :: String -> (String -> Update) -> Argument
requiredArgument = flip Console.ReqArg

unknownOptionWarning :: String -> String
unknownOptionWarning = mappend "unknown option " . quote

unexpectedArgumentWarning :: String -> String
unexpectedArgumentWarning = mappend "unexpected argument " . quote

quote :: String -> String
quote x = "`" <> x <> "'"

withNewline :: String -> String
withNewline x = x <> "\n"

applyUpdates :: [Update] -> Either String Config
applyUpdates = List.foldl' (>>=) $ Right defaultConfig
