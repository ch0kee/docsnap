{-# LANGUAGE OverloadedStrings #-}

module DocSnap.Snaplets
  ( vcInit
  , accProvInit
  ) where

import Snap
import Control.Concurrent.MVar
import Control.Monad
import qualified Data.Map as M 

import DocSnap.VersionControl
import DocSnap.AccessProvider
--------------------------------------------------------------------------------
-- | Verziókezelő snaplet inicializálása.
vcInit :: SnapletInit b (VersionControl)
vcInit = makeSnaplet "vc" "Version Control Snaplet" Nothing $ do
    repos <- liftIO $ newMVar []
    return $ VersionControl repos
    
    
--------------------------------------------------------------------------------
-- | Hozzáféréskezelő snaplet inicializálása.
accProvInit :: SnapletInit b (AccessProvider k l a)
accProvInit = makeSnaplet "accProv" "Access Provider Snaplet" Nothing $ do
    accMap <- liftIO $ newMVar M.empty
    return $ AccessProvider accMap
