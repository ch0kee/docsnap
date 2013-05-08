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
import DocSnap.Repository

--------------------------------------------------------------------------------
-- | Az alkalmazás állapota
data App = App
    { _heist :: Snaplet (Heist App)
    , _session :: Snaplet SessionManager
    , _repository :: Snaplet Repository
--    , _database :: Snaplet MongoDB
    }

makeLenses ''App

--------------------------------------------------------------------------------
-- | Segédpéldányok a könnyebb hozzáférés céljából
instance HasHeist App where
  heistLens = subSnaplet heist

instance HasRepository (Handler b App) where
  getRepository = with repository get
  modifyRepository f = with repository (modify f)

{- jövőbeni fejlesztéshez
instance HasMongoDB App where
   getMongoDB app = view snapletValue (view database app)
-}

--------------------------------------------------------------------------------
type AppHandler = Handler App App


