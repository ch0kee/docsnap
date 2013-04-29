{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}

module DocSnapServer where

import System.IO
import    Snap.Snaplet
import qualified Data.ByteString as B
import    Data.IORef
import    Data.Map (Map)
import qualified Data.Map as M
import Data.Aeson.TH
import qualified Data.Aeson as A
import  Control.Monad.Trans
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Serialize 
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
--we can use tvar for the map, since
--we dont expect document-create-overflow

--todo: we use parametrized types to lift side effects into user code
--by using 'a' instead of 'MVar'
import Internal.Types
import Application --leginkább a doclens miatt
import Control.Concurrent.MVar.Strict


clearDeadSessionTimeout = 3000000
minSessionId = 0
maxSessionId = 9999999

logDSS :: (MonadIO m) => String -> m ()
logDSS s = liftIO $ putStrLn ("** " ++ s)


tryAccessAs :: MonadIO m => DocumentHost -> SharedKey -> AccessRight ->  m (Maybe MDocument)
tryAccessAs dh sk ar = liftIO $ do
    putStrLn ("try access with sk=" ++ T.unpack sk)
    shareMap <- readMVar (shares dh)
    return $ case M.lookup sk shareMap of
        Just acc -> trace "access granted" $ accessDocument acc ar
        Nothing -> trace "no such access" $ Nothing
  where
    accessDocument :: DocumentAccess -> AccessRight -> Maybe MDocument
    accessDocument (DocumentAccess (accr, doc)) r | accr == r = trace "access ok" $ Just doc
    accessDocument _ _ = trace "wrong access" $ Nothing
      

documentHostInit :: SnapletInit b DocumentHost
documentHostInit = makeSnaplet "dochost" "DocumentHost Snaplet" Nothing $ do
    (dm,sm) <- liftIO $ liftM2 (,) (newMVar []) (newMVar M.empty)
    --liftIO . forkIO $ clearDeadSessions dm    
    return $ DocumentHost dm sm

--véletlenszerű megosztókulcsok generálása  
generateSharedKeys :: RandomGen g => g -> [SharedKey]
generateSharedKeys g = map (T.pack . UUID.toString) (randoms g :: [UUID])


-- | Új, üres dokumentum
emptyDocument :: Document
emptyDocument = Document {revisions=[], chatLog=[]}

-- | Új dokumentum létrehozása
createDocument :: MonadIO m => DocumentHost -> m (MDocument)
createDocument dh = do
    let mdocs = documents dh
    liftIO $ trace "creating..." $ do
        mnewDoc <- newMVar emptyDocument
        docs <- takeMVar mdocs
        putMVar mdocs (mnewDoc:docs)
        return mnewDoc

-- | Dokumentum megosztása, ha még nincs megosztva
shareDocument :: MonadIO m =>
                 DocumentHost ->
--               ^^
                 DocumentAccess -> m (SharedKey)
shareDocument dh dacc = liftIO $ trace "sharing ..." $ do
  shareMap <- takeMVar $ shares dh
  --putMVar (shares dh) $ updatedMap ar doc shareMap
  gen <- newStdGen
  let (newShareMap, sk) = case locateMapKey dacc shareMap of
        Just oldSk -> (shareMap, oldSk)  --már meg van osztva
        Nothing -> insertNewShare dacc shareMap (generateSharedKeys gen)
  putMVar (shares dh) newShareMap
  trace ("shared as " ++ T.unpack sk) $ return () 
  return sk
  where
    --beszúr egy új dokumentumot, egyedi azonosítóval
    insertNewShare :: DocumentAccess -> ShareMap -> [SharedKey] -> (ShareMap, SharedKey)   
    insertNewShare dacc shareMap (sk:moreSk) = case M.lookup sk shareMap of
      Nothing -> (M.insert sk dacc shareMap, sk) --új, egyedi kulcs
      Just _  -> insertNewShare dacc shareMap moreSk --kulcsütközés

locateMapKey :: Eq a => a -> Map k a -> Maybe k
locateMapKey v m = locateMapKey' . dropWhile ((/=v) . snd)  $  M.toList m
  where
    locateMapKey' [] = Nothing
    locateMapKey' (x:_) = Just (fst x)

       
getRevisions :: MonadIO m => MDocument -> m [Revision]
getRevisions mdoc = do
  logDSS "getting revisions"
  doc <- liftIO . readMVar $ mdoc
  logDSS "revisions gotten"
  return $ revisions doc


--WARNING
appendRevision :: MonadIO m => Revision -> MDocument -> m ()
appendRevision r mdoc = do
  logDSS "appending revision"
  liftIO $ do
    doc <- takeMVar $ mdoc
    putMVar mdoc (doc {revisions=(revisions doc ++ [r])})
  logDSS "revision appended"


compatible :: [PackedEdit] -> [PackedEdit] -> Bool
compatible = undefined -- True

sequentialMerge :: [SingleEdit] -> [SingleEdit] -> [SingleEdit]
sequentialMerge = undefined --seqMergeEdits
      
parallelMerge :: (Revision, Revision) -> (Revision, Revision)
parallelMerge (Revision v1 e1, Revision v2 e2) = undefined

seqMergeRevisions :: [Revision] -> Maybe Revision
seqMergeRevisions [] = Nothing
seqMergeRevisions [r] = Just r
seqMergeRevisions revs = Just $ Revision {
        version=(version . last) revs
      , edits=packEdit . foldl seqMergeEdits [] . map (unpackEdit . edits) $ revs
    }
  where
--    seqMergePEdits :: [PackedEdit] -> [PackedEdit] -> [PackedEdit] 

    seqMergeEdits :: [SingleEdit] -> [SingleEdit] -> [SingleEdit]
    seqMergeEdits (SR:ls) r = (SR:seqMergeEdits ls r)
    seqMergeEdits l (SI c:rs) = (SI c: seqMergeEdits l rs)
    
    seqMergeEdits (SP:ls) (SP:rs) = SP:seqMergeEdits ls rs
    seqMergeEdits (SP:ls) (SR:rs) = SR:seqMergeEdits ls rs
    --seqMergeEdits l@(SP:ls) (SI c:rs) = (SI c:seqMergeEdits l rs)

    seqMergeEdits (SI c:ls) (SP:rs) = (SI c:seqMergeEdits ls rs)
    seqMergeEdits (SI c:ls) (SR:rs) = seqMergeEdits ls rs
    --seqMergeEdits l@(SI _:ls) (SI c:rs) = (SI c:seqMergeEdits l rs)
    seqMergeEdits [] r = r

    unpackEdit :: [PackedEdit] -> [SingleEdit]
    unpackEdit (R 0:rest) = unpackEdit rest 
    unpackEdit (R n:rest) = SR: unpackEdit (R (n-1):rest)
    unpackEdit (P 0:rest) = unpackEdit rest 
    unpackEdit (P n:rest) = SP: unpackEdit (P (n-1):rest)
    unpackEdit (I "":rest) = unpackEdit rest 
    unpackEdit (I (c:str):rest) = SI c: unpackEdit (I str:rest)
--    unpackEdit (I str:rest) = map SI str ++ unpackEdit rest
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


latestVersion :: [Revision] -> Version
latestVersion [] = 0
latestVersion rs = version (last rs)

--változások adott verzió után
after :: Version -> [Revision] -> [Revision]
after v rs = dropWhile (\r -> (version r) <= v) rs

-- rak [+2:ab|=3] abrak
commit :: MonadIO m => MDocument -> Revision -> m Revision
commit mdoc rev = do
  logDSS "committing"
  revs <- getRevisions mdoc --ezt meg a commitot atomi módon kell
  resp <- commit' rev revs --és itt kell a sorbafűzést elvégezni
  logDSS "committed"
  return resp
    where
      commit' :: MonadIO m => Revision -> [Revision] -> m Revision
      --checkout only todo:v == latestversion
      commit' (Revision v []) revs = do
        let retRev = seqMergeRevisions $ after v revs
        return $ maybe (Revision v []) id retRev  
      commit' r@(Revision v es) revs
        | latestVersion revs == v = do --can commit
            appendRevision (Revision (v+1) es) mdoc
            return $ Revision (v+1) []
        | otherwise = do --can not commit
          let retRev = seqMergeRevisions $ after v revs  --ha ez Nothing, az hiba
          return $ maybe (Revision v []) id retRev

