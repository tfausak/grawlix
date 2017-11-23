{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Grawlix.Server (main) where

import Flow ((|>))
import Grawlix.Database
import Grawlix.Handler.GetHealthCheck
import Grawlix.Handler.GetLibraries
import Grawlix.Handler.GetModules
import Grawlix.Handler.GetPackages
import Grawlix.Handler.GetRevisions
import Grawlix.Handler.GetVersions

import qualified Hasql.Connection as Sql
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Gzip as Middleware
import qualified Network.Wai.Middleware.RequestLogger as Middleware
import qualified Servant


main :: IO ()
main = do
  connection <- getConnection
  connection |> applicationWith |> middleware |> Warp.runSettings settings


settings :: Warp.Settings
settings = Warp.defaultSettings
  |> Warp.setBeforeMainLoop (putStrLn "Starting server ...")
  |> Warp.setPort 8080
  |> Warp.setServerName mempty


middleware :: Wai.Middleware
middleware application = application
  |> Middleware.gzip Middleware.def
  |> Middleware.logStdout


applicationWith :: Sql.Connection -> Wai.Application
applicationWith connection = connection |> serverWith |> Servant.serve api


api :: Servant.Proxy Api
api = Servant.Proxy


type Api
  = GetHealthCheck
  Servant.:<|> GetPackages
  Servant.:<|> GetVersions
  Servant.:<|> GetRevisions
  Servant.:<|> GetLibraries
  Servant.:<|> GetModules

serverWith :: Sql.Connection -> Servant.Server Api
serverWith connection
  = getHealthCheckHandler connection
  Servant.:<|> getPackagesHandler connection
  Servant.:<|> getVersionsHandler connection
  Servant.:<|> getRevisionsHandler connection
  Servant.:<|> getLibrariesHandler connection
  Servant.:<|> getModulesHandler connection
