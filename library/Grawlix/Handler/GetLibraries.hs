{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Grawlix.Handler.GetLibraries
  ( GetLibraries
  , getLibrariesHandler
  ) where

import Grawlix.Handler.Common
import Grawlix.Query.SelectLibraries
import Grawlix.Type.LibraryId
import Grawlix.Type.PackageName
import Grawlix.Type.Revision
import Grawlix.Type.Version

type GetLibraries
   = "packages"
     :> Capture "package" PackageName
     :> "versions"
     :> Capture "version" Version
     :> "revisions"
     :> Capture "revision" Revision
     :> "libraries"
     :> Get '[ JSON] [LibraryId]

getLibrariesHandler ::
     Connection -> PackageName -> Version -> Revision -> Handler [LibraryId]
getLibrariesHandler connection package version revision =
  liftIO $ runQuery connection selectLibraries (package, version, revision)
