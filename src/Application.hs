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
import Control.Monad.State (get)

import DocSnapServer

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _sessionLens :: Snaplet SessionManager
    , _revLens :: Snaplet (RevisionControl)
    }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

instance HasRevisionControl (Handler App App) where
  getRevisionControlState = with revLens get

------------------------------------------------------------------------------
type AppHandler = Handler App App


