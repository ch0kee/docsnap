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
import Control.Concurrent.MVar

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
      

-- | A clearDeadSessions függvény egy olyan akciót hoz létre, amelyik
-- adott időközönként eldobja a nem frissített munkameneteket
clearDeadSessions :: MVar [MDocument] -> IO ()
clearDeadSessions mdocs = forever $ do
  threadDelay 3000000
  putStrLn "clearing dead sessions..."
  docs <- readMVar mdocs
  forM_ docs clearDeadSessions'
    where
      clearDeadSessions' :: MDocument -> IO ()
      clearDeadSessions' mdoc = do
        doc <- takeMVar mdoc
        let remainingEditors = filter touched (editors doc)
        let invalidatedEditors = map (\e -> e {touched=False}) remainingEditors
        putMVar mdoc $ doc {editors=invalidatedEditors}


documentHostInit :: SnapletInit b DocumentHost
documentHostInit = makeSnaplet "dochost" "DocumentHost Snaplet" Nothing $ do
  (dm,sm) <- liftIO $ liftM2 (,) (newMVar []) (newMVar M.empty)
  liftIO . forkIO $ clearDeadSessions dm    
  return $ DocumentHost dm sm

--véletlenszerű megosztókulcsok generálása  
generateSharedKeys :: RandomGen g => g -> [SharedKey]
generateSharedKeys g = map (T.pack . UUID.toString) (randoms g :: [UUID])


--véletlenszerű munkamenet azonosítók generálása
generateSessionIds :: RandomGen g => g -> [SessionId]
generateSessionIds = randomRs (0,2048)

--új résztvevő hozzáadása
addNewEditor :: MonadIO m => MDocument -> m (SessionId)
addNewEditor mdoc = liftIO $ do
  gen <- newStdGen
  let randomSids = generateSessionIds gen
  doc <- takeMVar mdoc
  let docSids = map sessId $ editors doc 
  let (sid:_) = until (\(x:_) -> x `notElem` docSids) tail randomSids
  putMVar mdoc $ doc { editors=editors doc ++ [Editor{sessId=sid,name="",touched=True}] }
  return sid
      
--visszaadja azt a listát, amiben a p predikátumot teljesítő
--elemekre alkalmazva van az f függvény
listWith :: (a -> Bool) -> (a -> a) -> [a] -> [a]
listWith p f (x:xs)
  | p x       = (f x:listWith p f xs) 
  | otherwise = (x:listWith p f xs)
listWith p f [] = []

--visszaadja azt a listát, amiben az első p predikátumot teljesítő
--elemek alkalmazva van az f függvény
listWithFirst :: (a -> Bool) -> (a -> a) -> [a] -> ([a], Maybe a)
listWithFirst p f xs =
  case findIndex p xs of
    Nothing -> (xs, Nothing)
    Just i  -> composition' f (splitAt i xs)
  where
    composition' f (left, x:xs) = (left ++ [f x] ++ xs, Just $ f x)


  
--résztvevő érvényesítése
touchEditor :: MonadIO m => MDocument -> SessionId -> m (Maybe SessionId)
touchEditor mdoc oldSid = liftIO $ do
  trace ("old session id :" ++ show oldSid) $ return ()
  randomSids <- newStdGen >>= return . generateSessionIds
  doc <- takeMVar mdoc
  let docSids = map sessId $ editors doc
  trace ("session ids before touch " ++ show docSids) $ return () 
  let (newEditors, maybeNewSessionId) = listWithFirst ((==oldSid) . sessId) (update docSids randomSids) $ editors doc
  trace ("session ids AFTER touch " ++ show (map sessId newEditors)) $ return () 
  putMVar mdoc $ doc { editors=newEditors }
  return $ maybe Nothing (Just . sessId) maybeNewSessionId
    where
      update :: [SessionId] -> [SessionId] -> Editor -> Editor
      update docSids randomSids er = er { 
          touched=True --érvényesítjük
        , sessId=head $ until (\(x:_) -> x `notElem` docSids) tail randomSids
      }
      
      
emptyDocument :: Document
emptyDocument = Document {revisions=[], editors=[]}

--dokumentum létrehozása és megosztása
createDocument :: MonadIO m => DocumentHost -> m (MDocument)
createDocument dh = do
  let mdocs = documents dh
  liftIO $ trace "creating..." $ do
    mnewDoc <- newMVar emptyDocument
    docs <- takeMVar mdocs
    putMVar mdocs (mnewDoc:docs)
    return mnewDoc

shareDocument :: MonadIO m => DocumentHost -> DocumentAccess -> m (SharedKey)
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


--egymas utan kovetkezo reviziok osszefuzese egybe
--a.k.a. sequentialMerge
concatRevisions :: [Revision] -> Maybe Revision
concatRevisions [] = Nothing
concatRevisions (r:[]) = Just r
concatRevisions revs = Just $ concatRevisions' revs
  where
    concatRevisions' [r] = r
    concatRevisions' (Revision v1 es1: Revision v2 es2:rest) = concatRevisions' (Revision v2 (packEdit $ concatES upes1 upes2):rest)
      where
        upes1 = unpackEdit es1
        upes2 = unpackEdit es2
        
        concatES :: [SingleEdit] -> [SingleEdit] -> [SingleEdit]
        concatES (SP:ls) (SP:rs) = SP:concatES ls rs
        concatES (SP:ls) (SR:rs) = (SR:concatES ls rs)
        concatES l@(SP:ls) (SI c:rs) = (SI c:concatES l rs)

        concatES (SR:ls) r = (SR:concatES ls r)

        concatES (SI c:ls) (SP:rs) = (SI c:concatES ls rs)
        concatES (SI c:ls) (SR:rs) = concatES ls rs
        concatES l@(SI _:ls) (SI c:rs) = (SI c:concatES l rs)
        concatES [] r = r

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
      

latestVersion :: [Revision] -> Version
latestVersion [] = 0
latestVersion rs = version (last rs)

nextVersion :: [Revision] -> Version
nextVersion = (+1) . latestVersion

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
        let retRev = concatRevisions $ after v revs
        return $ maybe (Revision v []) id retRev  
      commit' r@(Revision v es) revs
        | latestVersion revs == v = do --can commit
            appendRevision (Revision (v+1) es) mdoc
            return $ Revision (v+1) []
        | otherwise = do --can not commit
          let retRev = concatRevisions $ after v revs  --ha ez Nothing, az hiba
          return $ maybe (Revision v []) id retRev

