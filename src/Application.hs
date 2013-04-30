{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
------------------------------------------------------------------------------
-- | Ez a modul definiálja az alkalmazás állapotát
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Data.IORef
import qualified Data.ByteString as B
import Control.Monad.State (get, modify, gets)--, put, modify)

import DocSnap.Internal.Types

------------------------------------------------------------------------------
data App = App
    { _heist :: Snaplet (Heist App)
    , _session :: Snaplet SessionManager
    , _repository :: Snaplet Repository
    }

makeLenses ''App

instance HasHeist App where
  heistLens = subSnaplet heist

instance HasRepository (Handler b App) where
  getRepository = with repository get
  modifyRepository f = with repository (modify f)


--instance HasRevisionControl (Handler App App) where
--  getRevisionControlState = with revLens get

------------------------------------------------------------------------------
type AppHandler = Handler App App


