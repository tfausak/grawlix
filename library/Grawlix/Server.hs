{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}

module Grawlix.Server where

import Flow ((|>))
import Grawlix.Database
import Grawlix.Types

import qualified Control.Monad.IO.Class as IO
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

type GetHealthCheck
  = "health-check"
  Servant.:> Servant.Get '[Servant.JSON] Bool

type GetPackages
  = "packages"
  Servant.:> Servant.Get '[Servant.JSON] [PackageName]

type GetVersions
  = "packages"
  Servant.:> Servant.Capture "package" PackageName
  Servant.:> "versions"
  Servant.:> Servant.Get '[Servant.JSON] [Version]

type GetRevisions
  = "packages"
  Servant.:> Servant.Capture "package" PackageName
  Servant.:> "versions"
  Servant.:> Servant.Capture "version" Version
  Servant.:> "revisions"
  Servant.:> Servant.Get '[Servant.JSON] [Revision]

type GetLibraries
  = "packages"
  Servant.:> Servant.Capture "package" PackageName
  Servant.:> "versions"
  Servant.:> Servant.Capture "version" Version
  Servant.:> "revisions"
  Servant.:> Servant.Capture "revision" Revision
  Servant.:> "libraries"
  Servant.:> Servant.Get '[Servant.JSON] [LibraryName]

type GetModules
  = "packages"
  Servant.:> Servant.Capture "package" PackageName
  Servant.:> "versions"
  Servant.:> Servant.Capture "version" Version
  Servant.:> "revisions"
  Servant.:> Servant.Capture "revision" Revision
  Servant.:> "libraries"
  Servant.:> Servant.Capture "library" LibraryName
  Servant.:> "modules"
  Servant.:> Servant.Get '[Servant.JSON] [ModuleName]


serverWith :: Sql.Connection -> Servant.Server Api
serverWith connection
  = getHealthCheckHandler connection
  Servant.:<|> getPackagesHandler connection
  Servant.:<|> getVersionsHandler connection
  Servant.:<|> getRevisionsHandler connection
  Servant.:<|> getLibrariesHandler connection
  Servant.:<|> getModulesHandler connection


getHealthCheckHandler :: Sql.Connection -> Servant.Handler Bool
getHealthCheckHandler connection = io (runQuery connection pingQuery ())


getPackagesHandler :: Sql.Connection -> Servant.Handler [PackageName]
getPackagesHandler connection = io (runQuery connection selectPackageNames ())


getVersionsHandler :: Sql.Connection -> PackageName -> Servant.Handler [Version]
getVersionsHandler connection package =
  io (runQuery connection selectVersions package)


getRevisionsHandler :: Sql.Connection -> PackageName -> Version -> Servant.Handler [Revision]
getRevisionsHandler connection package version =
  io (runQuery connection selectRevisions (package, version))


getLibrariesHandler :: Sql.Connection -> PackageName -> Version -> Revision -> Servant.Handler [LibraryName]
getLibrariesHandler connection package version revision =
  io (runQuery connection selectLibraries (package, version, revision))


getModulesHandler
  :: Sql.Connection
  -> PackageName
  -> Version
  -> Revision
  -> LibraryName
  -> Servant.Handler [ModuleName]
getModulesHandler connection package version revision library =
  io (runQuery connection selectModules (package, version, revision, library))


io :: IO a -> Servant.Handler a
io = IO.liftIO
