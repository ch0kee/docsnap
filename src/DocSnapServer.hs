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
import Serialize (parseRevision)
import Control.Concurrent.STM
import qualified Control.Monad.State as S
import Control.Monad (liftM, liftM2, ap)
import Control.Applicative
import qualified Data.Text as T
import Debug.Trace
import Data.UUID as UUID
import Control.Monad.Random --for random in STM
--we can use tvar for the map, since
--we dont expect document-create-overflow

--todo: we use parametrized types to lift side effects into user code
--by using 'a' instead of 'MVar'
import Internal.Types
import Application --leginkább a doclens miatt

tryAccessAsAuthor :: (MonadIO m, HasDocumentHost m) => SharedKey -> m (Maybe AuthorAccess)
tryAccessAsAuthor sk = do
  liftIO $ putStrLn ("try access with sk=" ++ T.unpack sk)
  dm <- liftIO . readTVarIO =<< return . authorShareMap =<< getDocumentHost 
  acc <- return $ M.lookup sk dm >>= Just . AuthorAccess sk
  case acc of
    Nothing -> liftIO (putStrLn "access failed")
    Just x -> return ()
  return acc

logDSS :: (MonadIO m) => String -> m ()
logDSS s = liftIO $ putStrLn ("** " ++ s)

emptyDocument :: Document
emptyDocument = Document [] 

documentHostInit :: SnapletInit b DocumentHost
documentHostInit = makeSnaplet "dochost" "DocumentHost Snaplet" Nothing $ do
  (asm,rsm) <- liftIO . atomically $ liftM2 (,) (newTVar M.empty) (newTVar M.empty)
  return $ DocumentHost asm rsm

--véletlenszerű kulcs generálása  
generateSharedKey :: (Monad m, RandomGen g) => RandT g m T.Text
generateSharedKey = liftM (T.pack . UUID.toString) getRandom

--ez inkabb egy olyan monadban ugykodjon, ahol biztos nem csesztet mast
createDocument :: HasDocumentHost m => m AuthorAccess
createDocument = do
  tm <- return . authorShareMap =<< getDocumentHost
  liftIO $ do
    logDSS "creating document"
    gen <- newStdGen
    newdoc <- newTVarIO emptyDocument
    acc <- atomically $ evalRandT (insertWithUniqueKey newdoc tm) gen
    logDSS "document created"
    return acc
  where
    insertWithUniqueKey :: TDocument -> TShareMap -> RandT StdGen STM AuthorAccess
    insertWithUniqueKey newdoc tm = do
      sk <- generateSharedKey
      lift $ do
        m <- readTVar tm
        case M.lookup sk m of
          Just _ -> retry --in case of 'wonders'
          _      -> do
            writeTVar tm (M.insert sk newdoc m) 
      return $ AuthorAccess sk newdoc
            
           
getRevisions :: MonadIO m => AuthorAccess -> m [Revision]
getRevisions a = liftM revisions $ do
  logDSS "getting revisions"
  revs <- liftIO . readTVarIO . doc $ a
  logDSS "revisions gotten"
  return revs


--WARNING
appendRevision :: HasDocumentHost m => Revision -> AuthorAccess -> m ()
appendRevision r acc = do
  logDSS "appending revision"
  liftIO . atomically $ modifyTVar' (doc acc) (\d -> Document $ (revisions d ++ [r]))
  logDSS "revision appended"


--egymas utan kovetkezo reviziok osszefuzese egybe
concatRevisions :: [Revision] -> Maybe Revision
concatRevisions [] = Nothing
concatRevisions (r:[]) = Just r
concatRevisions revs = Just $ concatRevisions' revs
  where
    concatRevisions' [r] = r
    concatRevisions' (Revision (es1,v1): Revision (es2,v2):rest) = concatRevisions' (Revision (packEdit $ concatES upes1 upes2, v2):rest)
      where
        upes1 = unpackEdit es1
        upes2 = unpackEdit es2
        
        concatES :: [SingleEdit] -> [SingleEdit] -> [SingleEdit]
        concatES (Preserve:ls) (Preserve:rs) = Preserve:concatES ls rs
        concatES (Preserve:ls) (Remove:rs) = (Remove:concatES ls rs)
        concatES l@(Preserve:ls) (Insert c:rs) = (Insert c:concatES l rs)

        concatES (Remove:ls) r = (Remove:concatES ls r)

        concatES (Insert c:ls) (Preserve:rs) = (Insert c:concatES ls rs)
        concatES (Insert c:ls) (Remove:rs) = concatES ls rs
        concatES l@(Insert _:ls) (Insert c:rs) = (Insert c:concatES l rs)
        concatES [] r = r

        unpackEdit :: [PackedEdit] -> [SingleEdit]
        unpackEdit (Removes 0:rest) = unpackEdit rest 
        unpackEdit (Removes n:rest) = Remove: unpackEdit (Removes (n-1):rest)
        unpackEdit (Preserves 0:rest) = unpackEdit rest 
        unpackEdit (Preserves n:rest) = Preserve: unpackEdit (Preserves (n-1):rest)
        unpackEdit (Inserts "":rest) = unpackEdit rest 
        unpackEdit (Inserts (c:str):rest) = Insert c: unpackEdit (Inserts str:rest)
        unpackEdit [] = []
        
        packEdit :: [SingleEdit] -> [PackedEdit]
        packEdit es = snd $ packEdit' (es, [])
        packEdit' (Remove:ur, Removes n:pr) = packEdit' (ur, Removes (n+1):pr)
        packEdit' (Remove:ur, p) = packEdit' (ur, Removes 1:p)
        
        packEdit' (Preserve:ur, Preserves n:pr) = packEdit' (ur, Preserves (n+1):pr)
        packEdit' (Preserve:ur, p) = packEdit' (ur, Preserves 1:p)
        
        packEdit' (Insert c:ur, Inserts s:pr) = packEdit' (ur, Inserts (s++[c]):pr)
        packEdit' (Insert c:ur, p) = packEdit' (ur, Inserts (c:[]):p)  
        
        packEdit' ([], p) = ([], p)
      


-----------
{-
concatRevisionsIO :: [Revision] -> IO (Maybe Revision)
concatRevisionsIO [] = return Nothing
concatRevisionsIO (r:[]) = return $ Just r
concatRevisionsIO revs = do
  logDSS "prepare to concat.."
  cct <- concatRevisions' revs
  return $ Just cct 
  where
    concatRevisions' [r] = return r
    concatRevisions' (Revision (es1,v1): Revision (es2,v2):rest) = do
      let conc = concatES upes1 upes2
      liftIO $ putStrLn "concatES"
      liftIO $ putStrLn $ show conc
      let packed = packEdit conc
      liftIO $ putStrLn "packEdit"
      liftIO $ putStrLn $ show packed
      rs <- concatRevisions' (Revision (packEdit $ concatES upes1 upes2, v2):rest)
      logDSS "concat done"
      return rs
      where
        upes1 = unpackEdit es1
        upes2 = unpackEdit es2
        
        concatES :: [SingleEdit] -> [SingleEdit] -> [SingleEdit]
        concatES (Preserve:ls) (Preserve:rs) = Preserve:concatES ls rs
        concatES (Preserve:ls) (Remove:rs) = (Remove:concatES ls rs)
        concatES l@(Preserve:ls) (Insert c:rs) = (Insert c:concatES l rs)

        concatES (Remove:ls) r = (Remove:concatES ls r)

        concatES (Insert c:ls) (Preserve:rs) = (Insert c:concatES ls rs)
        concatES (Insert c:ls) (Remove:rs) = concatES ls rs
        concatES l@(Insert _:ls) (Insert c:rs) = (Insert c:concatES l rs)
        concatES [] r = r

        unpackEdit :: [PackedEdit] -> [SingleEdit]
        unpackEdit (Removes 0:rest) = unpackEdit rest 
        unpackEdit (Removes n:rest) = Remove: unpackEdit (Removes (n-1):rest)
        unpackEdit (Preserves 0:rest) = unpackEdit rest 
        unpackEdit (Preserves n:rest) = Preserve: unpackEdit (Preserves (n-1):rest)
        unpackEdit (Inserts "":rest) = unpackEdit rest 
        unpackEdit (Inserts (c:str):rest) = Insert c: unpackEdit (Inserts str:rest)
        unpackEdit [] = []
        
        packEdit :: [SingleEdit] -> [PackedEdit]
        packEdit es = snd $ packEdit' (es, [])
        packEdit' (Remove:ur, Removes n:pr) = packEdit' (ur, Removes (n+1):pr)
        packEdit' (Remove:ur, p) = packEdit' (ur, Removes 1:p)
        
        packEdit' (Preserve:ur, Preserves n:pr) = packEdit' (ur, Preserves (n+1):pr)
        packEdit' (Preserve:ur, p) = packEdit' (ur, Preserves 1:p)
        
        packEdit' (Insert c:ur, Inserts s:pr) = packEdit' (ur, Inserts (s ++ [c]):pr)
        packEdit' (Insert c:ur, p) = packEdit' (ur, Inserts (c:[]):p)  
        
        packEdit' ([], p) = ([], p)
--a verziók növekvő sorrendben vannak!!!
-}


latestVersion :: [Revision] -> Version
latestVersion [] = 0
latestVersion rs = (\(Revision (es,v)) -> v) (last rs)

un_revision (Revision (es,v)) = (es,v)

nextVersion :: [Revision] -> Version
nextVersion = (+1) . latestVersion

--változások adott verzió után
after :: Version -> [Revision] -> [Revision]
after v rs = dropWhile (\r -> (snd $ un_revision r) <= v) rs

--commit2 :: AuthorAccess -> Revision -> m Result
--commit2 aacc 

-- rak [+2:ab|=3] abrak
commit :: HasDocumentHost m => B.ByteString -> AuthorAccess -> m Revision
commit cdata acc = do
  logDSS "committing"
--  liftIO $ putStrLn $ ("%%% commit " ++ cdata)
  case parseRevision cdata of
    Nothing -> undefined --error msg
    Just rev -> do
      revs <- getRevisions acc
      resp <- commit' rev revs
      logDSS "committed"
      return resp
  where
    commit' :: HasDocumentHost m => Revision -> [Revision] -> m Revision
    --checkout only todo:v == latestversion
    commit' (Revision ([],v)) revs = do
      let retRev = concatRevisions $ after v revs
      return $ maybe (Revision ([],v)) id retRev  
    commit' r@(Revision (es,v)) revs
      | latestVersion revs == v = do --can commit
          appendRevision (Revision (es,v+1)) acc
          return $ Revision ([], v+1)
      | otherwise = do --can not commit
        let retRev = concatRevisions $ after v revs  --ha ez Nothing, az hiba
        return $ maybe (Revision ([],v)) id retRev
{-
packEdits :: [Edit] -> [PackedEdit]
packEdits edits = reverse $ foldl add' [] edits
  where
    add' :: [PackedEdit] -> Edit -> [PackedEdit]
    add' (p:ps) e = add p e: add' ps e
    add' [] e =

    add :: [PackedEdit] -> Edit -> [PackedEdit]
    add (Inserts s) (Insert c) = Inserts (s ++ show c)
    add (Preserves n) Preserve = Preserves(n+1)
    add (Removes n) Remove = Removes (n+1)
    add _ (Insert c) = Inserts $ show c
    add _ Preserve = Preserves 1
    add _ Remove = Removes 1

packRevision :: Revision -> PackedRevision
packRevision (Revision (es, version)) = PackedRevision (packEdits es, version)
-}

