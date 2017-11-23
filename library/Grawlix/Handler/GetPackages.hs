{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Grawlix.Handler.GetPackages
  ( GetPackages
  , getPackagesHandler
  ) where

import Grawlix.Handler.Common
import Grawlix.Query.SelectPackageNames
import Grawlix.Type.PackageName

type GetPackages
   = "packages"
     :> Get '[ JSON] [PackageName]

getPackagesHandler :: Connection -> Handler [PackageName]
getPackagesHandler connection =
  liftIO (runQuery connection selectPackageNames ())
