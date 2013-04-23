{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Data.IORef
import qualified Data.ByteString as B
import Control.Monad.State (get, modify)--, put, modify)

import Internal.Types

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _session :: Snaplet SessionManager
    --, _revLens :: Snaplet (RevisionControl)
    , _docHost :: Snaplet DocumentHost
    }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasDocumentHost (Handler b App) where
  getDocumentHost = with docHost get
  modifyDH f = with docHost (modify f)


--instance HasRevisionControl (Handler App App) where
--  getRevisionControlState = with revLens get

------------------------------------------------------------------------------
type AppHandler = Handler App App


