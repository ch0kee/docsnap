{-# LANGUAGE GADTs, KindSignatures #-}

module DocSnap.AccessProvider where

import qualified Data.Map as M
import Data.Maybe
import Control.Concurrent.MVar
import Control.Monad.Trans
import  Data.Ord
--import Data.UUID as UUID
import Control.Monad.Random
import Control.Monad.State 

import Debug.Trace

import           Snap.Snaplet

-- | Hozzáféréseket biztosít
-- | k: kulcs típusa
-- | l: hozzáférési szintek
data AccessProvider k l a = AccessProvider {
   accessMap :: MVar (M.Map k (Access l a)) 
}

--------------------------------------------------------------------------------
-- | Hozzáférés
-- | l: hozzáférési leíró
data Access l a = Access l a
  deriving (Eq, Show)
  
--------------------------------------------------------------------------------
-- | Hozzáférés dokumentumhoz megosztási linken keresztül
tryAccess :: (Show k, Ord k) => k -> Handler b (AccessProvider k l a) (Maybe (Access l a))
tryAccess key = do
    am <- gets (readMVar . accessMap)
    liftIO am >>= return . M.lookup key 


--------------------------------------------------------------------------------
-- | elemhez hozzáférés kapcsolása
makeAccessible :: (Eq l, Eq a, Random k, Ord k)
              => l
              -> a 
              -> Handler b (AccessProvider k l a) k
makeAccessible  level entity = do
    am <- gets accessMap
    key <- liftIO $ modifyMVar am $ \accMap -> do
        randomKeys <- generateKeys'
        return $ case locateMapKey (Access level entity) accMap of
          Just key -> (accMap, key)
          Nothing -> storeNewAccess (Access level entity) accMap randomKeys
    return key
  where
    --beszúr egy új dokumentumot, egyedi azonosítóval
    storeNewAccess (Access level entity) accMap (key:moreKey) = case M.lookup key accMap of
        Nothing -> (M.insert key (Access level entity) accMap, key) --új, egyedi kulcs
        Just _  -> storeNewAccess (Access level entity) accMap moreKey --kulcsütközés
    --véletlenszerű megosztókulcsok generálása  
    generateKeys' =  evalRandIO getRandoms

locateMapKey :: Eq a => a -> M.Map k a -> Maybe k
locateMapKey v m = locateMapKey' . dropWhile ((/=v) . snd)  $  M.toList m
  where
    locateMapKey' [] = Nothing
    locateMapKey' (x:_) = Just (fst x)
    


