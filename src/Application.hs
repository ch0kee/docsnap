--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
--------------------------------------------------------------------------------
-- | Ez a modul definiálja az alkalmazás állapotát
module Application where

--------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Data.IORef
import qualified Data.ByteString as B
import Control.Monad.State (get, modify, gets)--, put, modify)
--import Snap.Snaplet.MongoDB
import DocSnap.Internal.Types
--import DocSnap.Repository

import DocSnap.VersionControl
import DocSnap.AccessProvider
import DocSnap.Document
import Data.UUID as UUID
--------------------------------------------------------------------------------
-- | Az alkalmazás állapota
data App = App
    { _heist :: Snaplet (Heist App)
  --  , _repository :: Snaplet Repository
    , _versionControl    :: Snaplet DocumentVersionControl
    , _accessProvider  :: Snaplet DocumentAccessProvider
--    , _database :: Snaplet MongoDB
    }

makeLenses ''App

--------------------------------------------------------------------------------
-- | Segédpéldányok a könnyebb hozzáférés céljából
instance HasHeist App where
  heistLens = subSnaplet heist



--------------------------------------------------------------------------------
type AppHandler = Handler App App


