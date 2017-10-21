{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Grawlix.Server where

import Flow ((|>))
import Grawlix.Database

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


type Api = GetHealthCheck
type GetHealthCheck = "health-check" Servant.:> Servant.Get '[Servant.JSON] Bool


serverWith :: Sql.Connection -> Servant.Server Api
serverWith connection = getHealthCheck connection


getHealthCheck :: Sql.Connection -> Servant.Handler Bool
getHealthCheck connection = IO.liftIO (runQuery connection pingQuery ())
