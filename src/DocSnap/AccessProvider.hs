--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module DocSnap.AccessProvider
  ( AccessProvider (..)
  , Access (..)
  , tryAccess
  , makeAccessible
  , initAccessProvider
  , ShareRequest(..)
  , ShareResponse(..)
  ) where

--------------------------------------------------------------------------------
import qualified Data.Map as M
import           Data.Maybe
import           Control.Concurrent.MVar
import           Control.Monad.Trans
import           Data.Ord
import           Control.Monad.Random
import           Control.Monad.State 
import           Snap.Snaplet
--------------------------------------------------------------------------------
import Data.Aeson.TH

--------------------------------------------------------------------------------
-- | Hozzáféréseket biztosít
-- | k: kulcs típusa
-- | l: hozzáférési szintek
-- | a: tárolt entitás
data AccessProvider k l a = AccessProvider {
   accessMap :: MVar (M.Map k (Access l a)) 
}


       
--------------------------------------------------------------------------------
-- | Hozzáféréskezelő snaplet inicializálása.
initAccessProvider :: SnapletInit b (AccessProvider k l a)
initAccessProvider = makeSnaplet "accProv" "Access Provider Snaplet" Nothing $ do
    accMap <- liftIO $ newMVar M.empty
    return $ AccessProvider accMap

--------------------------------------------------------------------------------
-- | Hozzáférés
data Access l a = Access l a
  deriving (Eq, Show)
  
--------------------------------------------------------------------------------
-- | Hozzáférés entitáshoz
tryAccess :: (Ord k) => k -> Handler b (AccessProvider k l a) (Maybe (Access l a))
tryAccess key = do
    am <- gets (readMVar . accessMap)
    liftIO am >>= return . M.lookup key 


--------------------------------------------------------------------------------
-- | Entitáshoz hozzáférés letárolása
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
    --beszúr egy új entitást a megfelelő hozzáféréssel, egyedi azonosítóval
    storeNewAccess (Access level entity) accMap (key:moreKey) = case M.lookup key accMap of
        Nothing -> (M.insert key (Access level entity) accMap, key) --új, egyedi kulcs
        Just _  -> storeNewAccess (Access level entity) accMap moreKey --kulcsütközés
    --véletlenszerű megosztókulcsok generálása  
    generateKeys' =  evalRandIO getRandoms


--------------------------------------------------------------------------------
-- Megkeres egy értékhez tartozó első kulcsot
locateMapKey :: Eq a => a -> M.Map k a -> Maybe k
locateMapKey v m = locateMapKey' . dropWhile ((/=v) . snd)  $  M.toList m
  where
    locateMapKey' [] = Nothing
    locateMapKey' (x:_) = Just (fst x)
    
  
--------------------------------------------------------------------------------
-- | Megosztási kérelem
data ShareRequest = ShareRequest
    { shareRequest_type :: String}
  deriving(Show)

--------------------------------------------------------------------------------
-- | Megosztási link
data ShareResponse = ShareResponse 
    { shareResponse_link :: String }  
  deriving(Show)


$(deriveJSON (drop 14) ''ShareResponse)
$(deriveJSON (drop 13) ''ShareRequest)
