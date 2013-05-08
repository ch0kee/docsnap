--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------------
-- | A verziókezelés implementációja található ebben a modulban, illetve a
-- használatához szükséges interfész.
module DocSnap.Repository
  ( repositoryInit
  , createDocument
  , access
  , shareDocument
  , seqMergeRevisions
  , getRevisions
  , update
  , receiveChatMessages
  , sendChatMessage
  , Repository (..)
  , Document (..)
  , HasRepository
  , getRepository
  , modifyRepository
  , PackedEdit (..)
  , SharedKey
  , ChatMessage (..)
  , DocumentAccess (..)
  , Access(..)
  , Revision(..)
  , ShareRequest(..)
  , ShareResponse(..)
  , AccessRight(..)
  , Request(..)
  , UpdateResponse(..)
  ) where

--------------------------------------------------------------------------------
import System.IO
import    Snap.Snaplet
import qualified Data.ByteString as B
import    Data.IORef
import    Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Aeson as A
import  Control.Monad.Trans
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import DocSnap.Serialize 
import Control.Concurrent.STM
import qualified Control.Monad.State as S
import Control.Monad (liftM, liftM2, liftM3, ap)
import Control.Applicative
import qualified Data.Text as T
import Debug.Trace
import Data.UUID as UUID
import Control.Monad.Random --for random in STM
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, forM_)
import Data.List (findIndex)
import DocSnap.Internal.Types
--import Application      -- repository miatt
import Control.Concurrent.MVar
import  Data.Aeson.TH

import DocSnap.Internal.Utilities
--------------------------------------------------------------------------------
import DocSnap.Repository.Types

--------------------------------------------------------------------------------
-- | Repository snaplet inicializálása.
repositoryInit :: SnapletInit b Repository
repositoryInit = makeSnaplet "repository" "Repository Snaplet" Nothing $ do
    (dm,sm) <- liftIO $ (,) `liftM` newMVar [] `ap` newMVar M.empty
    return $ Repository dm sm




--------------------------------------------------------------------------------
-- | Új dokumentum létrehozása. A függvény létrehoz egy új dokumentumot,
-- azt hozzáadja a tárolóhoz, majd visszatér a létrehozott dokumentummal.
createDocument :: (MonadIO m)
               => Repository    -- ^ tároló
               -> m (MDocument) -- ^ a létrehozott dokumentum
createDocument dh = do
    let mdocs = documents dh
    liftIO $ do
        mnewDoc <- newMVar emptyDocument
        docs <- takeMVar mdocs
        putMVar mdocs (mnewDoc:docs)
        return mnewDoc
  where
    emptyDocument :: Document
    emptyDocument = Document {revisions=[], chatLog=[]}

--------------------------------------------------------------------------------
-- | Hozzáférés dokumentumhoz megosztási linken keresztül
access :: (MonadIO m)
       => Repository  -- ^ tároló
       -> SharedKey   -- ^ megosztási link
       -> m Access    -- ^ hozzáférés
access dh sk = liftIO $ do
    shareMap <- readMVar (shares dh)
    case M.lookup sk shareMap of
        Just acc -> return $ Granted acc
        Nothing  -> return Denied


--------------------------------------------------------------------------------
-- | Dokumentum megosztása, ha még nincs megosztva
shareDocument :: (MonadIO m)
              => Repository     -- ^ tároló
              -> DocumentAccess -- ^ hozzáférési szint és dokumentum
              -> m (SharedKey)  -- ^ generált megosztási kulcs
shareDocument dh dacc = liftIO $ do
    sk <- modifyMVar (shares dh) $ \shareMap -> do
        randomKeys <- generateSharedKeys'
        return $ case locateMapKey dacc shareMap of
          Just oldSk -> (shareMap, oldSk)  --már meg van osztva
          Nothing -> insertNewShare dacc shareMap randomKeys
    return sk
  where
    --beszúr egy új dokumentumot, egyedi azonosítóval
    insertNewShare :: DocumentAccess -> ShareMap -> [SharedKey] -> (ShareMap, SharedKey)   
    insertNewShare dacc shareMap (sk:moreSk) = case M.lookup sk shareMap of
      Nothing -> (M.insert sk dacc shareMap, sk) --új, egyedi kulcs
      Just _  -> insertNewShare dacc shareMap moreSk --kulcsütközés
    --véletlenszerű megosztókulcsok generálása  
    generateSharedKeys' :: IO [SharedKey]
    generateSharedKeys' =  evalRandIO (getRandoms) >>= return . map (T.pack . UUID.toString)


--------------------------------------------------------------------------------
-- | Revíziók lekérdezése
getRevisions :: (MonadIO m)
             => MDocument     -- ^ dokumentum
             -> m [Revision]  -- ^ revíziók listája
getRevisions mdoc = do
  doc <- liftIO . readMVar $ mdoc
  return $ revisions doc

--------------------------------------------------------------------------------
-- | Revíziók rögzítése, illetve lekérdezése. Központi eljárás,
-- mely atomi módon megpróbálja rögzíteni a kliens által küldött revíziót.
-- Ha ez nem sikerül, mert időközben más kliens küldött be revíziót,
-- visszaküldi az elmaradt revíziókat összefűzve.
-- Ha a kliens nem küldött semmit, akkor csak visszaküldi az időközben beérkezett
-- revíziókat.
update :: (MonadIO m)
       => MDocument     -- ^ dokumentum
       -> Revision      -- ^ kliens által küldött revízió
       -> m Revision    -- ^ válaszban küldendő revízió
update mdoc rev = do
  doc <- liftIO $ takeMVar mdoc
  let revs = revisions doc
  (cliResp, newRevs) <- tryCommit rev revs --és itt kell a sorbafűzést elvégezni
  liftIO $ putMVar mdoc $ doc {revisions=newRevs}
  return cliResp
    where
      tryCommit :: (MonadIO m)
                => Revision    -- ^ kliens revíziója
                -> [Revision]  -- ^ repository revíziók
                -> m (Revision, [Revision])  -- ^ (kliensnek vissza, új repository revíziók)
      tryCommit (Revision v []) revs = do --nem küldött semmit, csak checkout
        let retRev = seqMergeRevisions $ after v revs
        return (maybe (Revision v []) id retRev, revs)
      tryCommit r@(Revision v es) revs
        | latestVersion revs == v = do --ő a legfrissebb, tároljuk a módosítását
            return (Revision (v+1) [], revs ++ [(Revision (v+1) es)])
        | otherwise = do --le van maradva, küldjük vissza amivel le van maradva
          let retRev = seqMergeRevisions $ after v revs
          return $ (maybe (Revision v []) id retRev, revs)


--------------------------------------------------------------------------------
-- | Chat üzenet küldése
sendChatMessage :: Document       -- ^ dokumentum
                -> ChatMessage    -- ^ chat üzenet
                -> Document       -- ^ dokumentum, amiben benne van a chat üzenet
sendChatMessage doc msg = doc {chatLog=withMessage msg (chatLog doc) }
  where
    withMessage msg [] = [(0, msg)]
    withMessage msg l@((nr,_):_) = (nr+1, msg):l


--------------------------------------------------------------------------------
-- | Chat üzenetek fogadása adott verziótól kezdve
receiveChatMessages :: Document                 -- ^ dokumentum
                    -> Version                  -- ^ kliens verziószáma
                    -> (Version,[ChatMessage])  -- ^ üzenetek a kliensverzió óta
receiveChatMessages doc v = receiveChatMessages' $ takeWhile ((>v)  . fst) $ chatLog doc
  where
    receiveChatMessages' [] = (v, [])
    receiveChatMessages' l@(x:_) = (fst x, map snd l)


--------------------------------------------------------------------------------
-- | A legfrissebb verziószám
latestVersion :: [Revision] -> Version
latestVersion [] = 0
latestVersion rs = version (last rs)


--------------------------------------------------------------------------------
-- | Változások csak egy adott verzió után
after :: Version -> [Revision] -> [Revision]
after v rs = dropWhile (\r -> (version r) <= v) rs


--------------------------------------------------------------------------------
-- | Revíziók szekvenciális összefűzése. Üres lista esetén
-- Nothing-gal tér vissza, egyébként Just <összefűzött revízió>-val
seqMergeRevisions :: [Revision]     -- ^ összefűzendő revíziók
                  -> Maybe Revision -- ^ összefűzött revízió
seqMergeRevisions [] = Nothing
seqMergeRevisions [r] = Just r
seqMergeRevisions revs = Just $ Revision {
        version=(version . last) revs
      , edits=packEdit . foldl seqMergeEdits [] . map (unpackEdit . edits) $ revs
    }
  where
    seqMergeEdits :: [SingleEdit] -> [SingleEdit] -> [SingleEdit]
    seqMergeEdits (SR:ls) r = (SR:seqMergeEdits ls r)
    seqMergeEdits l (SI c:rs) = (SI c: seqMergeEdits l rs)
    
    seqMergeEdits (SP:ls) (SP:rs) = SP:seqMergeEdits ls rs
    seqMergeEdits (SP:ls) (SR:rs) = SR:seqMergeEdits ls rs

    seqMergeEdits (SI c:ls) (SP:rs) = (SI c:seqMergeEdits ls rs)
    seqMergeEdits (SI c:ls) (SR:rs) = seqMergeEdits ls rs
    seqMergeEdits [] r = r

    unpackEdit :: [PackedEdit] -> [SingleEdit]
    unpackEdit (R 0:rest) = unpackEdit rest 
    unpackEdit (R n:rest) = SR: unpackEdit (R (n-1):rest)
    unpackEdit (P 0:rest) = unpackEdit rest 
    unpackEdit (P n:rest) = SP: unpackEdit (P (n-1):rest)
    unpackEdit (I "":rest) = unpackEdit rest 
    unpackEdit (I (c:str):rest) = SI c: unpackEdit (I str:rest)
    unpackEdit [] = []
    
    packEdit :: [SingleEdit] -> [PackedEdit]
    packEdit es = snd $ packEdit' (es, [])
    packEdit' (SR:ur, R n:pr) = packEdit' (ur, R (n+1):pr)
    packEdit' (SR:ur, p) = packEdit' (ur, R 1:p)
    
    packEdit' (SP:ur, P n:pr) = packEdit' (ur, P (n+1):pr)
    packEdit' (SP:ur, p) = packEdit' (ur, P 1:p)
    
    packEdit' (SI c:ur, I s:pr) = packEdit' (ur, I (s++[c]):pr)
    packEdit' (SI c:ur, p) = packEdit' (ur, I (c:[]):p)  
    
    packEdit' ([], p) = ([], p)
  

_P 0 rest = rest
_P n rest = (P n:rest)

_R 0 rest = rest
_R n rest = (R n:rest)

_I "" rest = rest
_I s rest = (I s:rest)

seqMergeNew :: [PackedEdit] -> [PackedEdit] -> [PackedEdit]
seqMergeNew (R n:ls) r = R n: seqMergeNew ls r --törölt szakasz már mindörökre törölve marad
seqMergeNew l ((I s):rs) = I s: seqMergeNew l rs --ha új beszúrás van, szúrjunk be 
seqMergeNew (P n:ls) (P m:rs) = P k: seqMergeNew (_P (n-k) ls) (_P (m-k) rs) --
  where k = min n m
seqMergeNew (P n:ls) (R m:rs) = R k: seqMergeNew (_P (n-k) ls) (_R (m-k) rs)
  where k = min n m
seqMergeNew (I s:ls) (P m:rs) = (I (take m s)):seqMergeNew (_I (drop m s) ls) (_P (m-(length s)) rs)
seqMergeNew (I s:ls) (R m:rs) = seqMergeNew (_I (drop m s) ls) (_R (m-(length s)) rs)
seqMergeNew l _ = l
