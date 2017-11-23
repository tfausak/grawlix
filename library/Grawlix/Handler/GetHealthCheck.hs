{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Grawlix.Handler.GetHealthCheck
  ( GetHealthCheck
  , getHealthCheckHandler
  ) where

import Grawlix.Handler.Common
import Grawlix.Query.SelectTrue

type GetHealthCheck
   = "health-check"
     :> Get '[ JSON] Bool

getHealthCheckHandler :: Connection -> Handler Bool
getHealthCheckHandler connection = liftIO (runQuery connection selectTrue ())
