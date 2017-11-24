module Grawlix.Options
  ( getOptions
  ) where

import Grawlix.Type.Options

import qualified Data.Text as Text
import qualified Options.Generic as Cli

getOptions :: IO Options
getOptions = Cli.getRecord $ Text.pack "Grawlix"
