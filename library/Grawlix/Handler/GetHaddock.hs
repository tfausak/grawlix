{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Grawlix.Handler.GetHaddock
  ( GetHaddock
  , getHaddockHandler
  ) where

import Grawlix.Handler.Common
import Grawlix.Type.ModuleName
import Grawlix.Type.PackageName
import Grawlix.Type.Version

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Lucid
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as Client

type GetHaddock
   = Capture "package" PackageName
     :> Capture "version" Version
     :> Capture "module" ModuleName
     :> Get '[ HTML] (Lucid.Html ())

getHaddockHandler ::
     PackageName -> Version -> ModuleName -> Handler (Lucid.Html ())
getHaddockHandler packageName version moduleName = do
  let url =
        concat
          [ "https://hackage.haskell.org/package/"
          , Text.unpack $ fromPackageName packageName
          , "-"
          , List.intercalate "." . map show $ fromVersion version
          , "/docs/"
          , List.intercalate "-" . map Text.unpack $ fromModuleName moduleName
          , ".html"
          ]
  request <- Client.parseUrlThrow url
  manager <- Client.newTlsManager
  response <- liftIO $ Client.httpLbs request manager
  pure . Lucid.toHtmlRaw $ Client.responseBody response
