module Grawlix.Handler.Common
  ( Capture
  , Connection
  , Get
  , Handler
  , HTML
  , JSON
  , (:>)
  , liftIO
  , runQuery
  ) where

import Control.Monad.IO.Class (liftIO)
import Grawlix.Database (runQuery)
import Hasql.Connection (Connection)
import Servant.API ((:>), Capture, Get, JSON)
import Servant.HTML.Lucid (HTML)
import Servant.Server (Handler)
