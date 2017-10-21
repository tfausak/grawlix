{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Grawlix.Server where

import Flow ((|>))
import Grawlix.Database
import Grawlix.Types

import qualified Control.Monad.IO.Class as IO
import qualified Data.Tagged as Tagged
import qualified Hasql.Connection as Sql
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Gzip as Middleware
import qualified Network.Wai.Middleware.RequestLogger as Middleware
import qualified Servant


instance Servant.FromHttpApiData a => Servant.FromHttpApiData (Tagged.Tagged t a) where
  parseUrlPiece x = fmap Tagged.Tagged (Servant.parseUrlPiece x)


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
  Servant.:<|> GetPackageVersions

type GetHealthCheck
  = "health-check"
  Servant.:> Servant.Get '[Servant.JSON] Bool

type GetPackages
  = "packages"
  Servant.:> Servant.Get '[Servant.JSON] [PackageName]

type GetPackageVersions
  = "packages"
  Servant.:> Servant.Capture "package" PackageName
  Servant.:> "versions"
  Servant.:> Servant.Get '[Servant.JSON] [Version]


serverWith :: Sql.Connection -> Servant.Server Api
serverWith connection
  = getHealthCheckHandler connection
  Servant.:<|> getPackagesHandler connection
  Servant.:<|> getVersionsHandler connection


getHealthCheckHandler :: Sql.Connection -> Servant.Handler Bool
getHealthCheckHandler connection = io (runQuery connection pingQuery ())


getPackagesHandler :: Sql.Connection -> Servant.Handler [PackageName]
getPackagesHandler connection = io (runQuery connection selectPackageNames ())


getVersionsHandler :: Sql.Connection -> PackageName -> Servant.Handler [Version]
getVersionsHandler connection package =
  io (runQuery connection selectVersions package)


io :: IO a -> Servant.Handler a
io = IO.liftIO
