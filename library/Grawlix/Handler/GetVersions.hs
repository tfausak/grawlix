{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Grawlix.Handler.GetVersions
  ( GetVersions
  , getVersionsHandler
  ) where

import Grawlix.Handler.Common
import Grawlix.Query.SelectVersions
import Grawlix.Type.PackageName
import Grawlix.Type.Version

type GetVersions
   = "packages"
     :> Capture "package" PackageName
     :> "versions"
     :> Get '[ JSON] [Version]

getVersionsHandler :: Connection -> PackageName -> Handler [Version]
getVersionsHandler connection package =
  liftIO $ runQuery connection selectVersions package
