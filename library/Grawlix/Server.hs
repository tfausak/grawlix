{-# LANGUAGE TypeOperators #-}

module Grawlix.Server
  ( main
  ) where

import Grawlix.Config
import Grawlix.Database
import Grawlix.Handler.GetHealthCheck
import Grawlix.Handler.GetLibraries
import Grawlix.Handler.GetModules
import Grawlix.Handler.GetPackages
import Grawlix.Handler.GetRevisions
import Grawlix.Handler.GetVersions
import Grawlix.Options
import Servant ((:<|>)((:<|>)))

import qualified Hasql.Connection as Sql
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Gzip as Middleware
import qualified Network.Wai.Middleware.RequestLogger as Middleware
import qualified Servant

main :: IO ()
main = do
  options <- getOptions
  config <- getConfig options
  connection <- getConnection config
  runServer connection

runServer :: Sql.Connection -> IO ()
runServer = Warp.runSettings settings . applyMiddleware . makeApplication

settings :: Warp.Settings
settings =
  Warp.setBeforeMainLoop (putStrLn "Starting server ...") . Warp.setPort 8080 $
  Warp.setServerName mempty Warp.defaultSettings

applyMiddleware :: Wai.Middleware
applyMiddleware = Middleware.gzip Middleware.def . Middleware.logStdout

makeApplication :: Sql.Connection -> Wai.Application
makeApplication = Servant.serve api . makeServer

api :: Servant.Proxy Api
api = Servant.Proxy

type Api
   = GetHealthCheck
     :<|> GetPackages
     :<|> GetVersions
     :<|> GetRevisions
     :<|> GetLibraries
     :<|> GetModules

makeServer :: Sql.Connection -> Servant.Server Api
makeServer connection =
  getHealthCheckHandler connection :<|> -- Force hindent newline.
  getPackagesHandler connection :<|>
  getVersionsHandler connection :<|>
  getRevisionsHandler connection :<|>
  getLibrariesHandler connection :<|>
  getModulesHandler connection
