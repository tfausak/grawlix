{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Grawlix.Handler.GetModules
  ( GetModules
  , getModulesHandler
  ) where

import Grawlix.Handler.Common
import Grawlix.Query.SelectModules
import Grawlix.Type.LibraryId
import Grawlix.Type.ModuleName
import Grawlix.Type.PackageName
import Grawlix.Type.Revision
import Grawlix.Type.Version

type GetModules
   = "packages"
     :> Capture "package" PackageName
     :> "versions"
     :> Capture "version" Version
     :> "revisions"
     :> Capture "revision" Revision
     :> "libraries"
     :> Capture "library" LibraryId
     :> "modules"
     :> Get '[ JSON] [ModuleName]

getModulesHandler ::
     Connection
  -> PackageName
  -> Version
  -> Revision
  -> LibraryId
  -> Handler [ModuleName]
getModulesHandler connection package version revision library =
  liftIO
    (runQuery connection selectModules (package, version, revision, library))
