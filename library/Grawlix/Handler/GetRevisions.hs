{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Grawlix.Handler.GetRevisions
  ( GetRevisions
  , getRevisionsHandler
  ) where

import Grawlix.Handler.Common
import Grawlix.Query.SelectRevisions
import Grawlix.Type.PackageName
import Grawlix.Type.Revision
import Grawlix.Type.Version

type GetRevisions
   = "packages"
     :> Capture "package" PackageName
     :> "versions"
     :> Capture "version" Version
     :> "revisions"
     :> Get '[ JSON] [Revision]

getRevisionsHandler ::
     Connection -> PackageName -> Version -> Handler [Revision]
getRevisionsHandler connection package version =
  liftIO $ runQuery connection selectRevisions (package, version)
