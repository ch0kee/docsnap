{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module DocSnapServer where

import Internal.Types
import    Snap.Snaplet
import qualified Data.ByteString as B
import    Data.IORef
import    Data.Map (Map)
import qualified Data.Map as M
import Data.Aeson.TH
import qualified Data.Aeson as A
import Control.Concurrent.MVar
import  Control.Monad.Trans
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Serialize (parseRevision, parseEditScript)
import Control.Concurrent.STM

--todo: we use parametrized types to lift side effects into user code
--by using 'a' instead of 'MVar'
data DocumentHost = DocumentHost {
  documents :: MVar (M.Map DocID Document)
, documentAccesses :: MVar (M.Map Accessor DocumentAccess)
}
--newtype DocumentAccess = Reader | Author
data DocumentAccess = DocumentAccess {
  permission :: Permission
, docid :: DocID
}
type DocumentRef = Integer
type DocID = Integer
type Accessor = String
data Permission = Author | Reader
type RevisionHistory = [Revision]
newtype Document = Document { revisionHistory :: RevisionHistory }
class (MonadIO m) => HasDocumentHost m
  where
    getDocumentHost :: m (DocumentHost)
    

newtype WriteAccess = WriteAccess Accessor
newtype ReadAccess = ReadAccess Accessor

{-
newtype DocumentWriter =
newtype DocumentReader =
class IDocumentContributor
  where
    
    

insertDoc :: HasDocumentHost m => Document -> m DocID
insertDoc d = do
  dh <- getDocumentHost
  let docs = documents dh
  docs <- liftIO $ takeMVar docs
  
  liftIO $ putMVar (documents dh)
-}
documentHostInit :: SnapletInit b DocumentHost
documentHostInit = makeSnaplet "dochost" "DocumentHost Snaplet" Nothing $ do
  docs <- liftIO $ newMVar (M.singleton 42 (Document []))
  das <- liftIO $ newMVar (M.singleton "42" (DocumentAccess { permission = Author, docid = 42 }))
  return DocumentHost { documents = docs, documentAccesses = das }
{-
createDocument :: HasDocumentHost m => m WriteAccess
createDocument = undefined
  where
    emptyDoc = Document { revisionHistory = [] }


getAccessURL

--todo: use newtype instead of type if possible
--todo: more informal error cause
--todo: returned value MUST not allow use inproper permissions
--todo: even if the code is wrong
tryAccessDocument :: HasDocumentHost m => Accessor -> m (Maybe (Permission,Document))
tryAccessDocument id = do
  dh <- getDocumentHost
  das <- liftIO $ readMVar (documentAccesses dh)
  docs <- liftIO $ readMVar (documents dh)
  case M.lookup id das of
    Nothing  -> return Nothing
    Just acc -> case M.lookup (docid acc) docs of
      Nothing -> return Nothing
      Just doc -> return $ Just (permission acc, doc)
-}

data RevisionControl = RevisionControl {
  rc_revisions :: MVar [Revision]
}




getTestDocument :: HasDocumentHost m => m (Document)
getTestDocument = do
  dh <- getDocumentHost
  docs <- liftIO $ readMVar (documents dh)
  case M.lookup 42 docs of
    Nothing -> error "NOT FOUND 42"
    Just d -> return d

--createNewDocument :: HasDocumentHost m -> m (Document)
--createNewDocument = do
--  docHost <- getDocumentHost
--  liftIO $ newMVar ()
--  M.insert 0 (
  

revisionControlInit :: SnapletInit b RevisionControl
revisionControlInit = makeSnaplet "revctrl" "Revision Control snaplet" Nothing $ do
  mv <- liftIO $ newMVar []
  return RevisionControl { rc_revisions = mv }

class (MonadIO m) => HasRevisionControl m
  where
    getRevisionControlState :: m (RevisionControl)

--TESZT KÓD
--reviziok adott indextol
getRevisions :: HasDocumentHost m => m [Revision]
getRevisions = do
  testdoc <- getTestDocument
  return $ revisionHistory testdoc

setRevisions revs = do
  dh <- getDocumentHost
  docs <- liftIO $ takeMVar (documents dh)
  let newMap = M.insertWith (\new old -> new) 42 (Document revs) docs
  liftIO $ putMVar (documents dh) newMap


--egymas utan kovetkezo reviziok osszefuzese egybe
concatRevisions :: [Revision] -> Revision
concatRevisions [] = Revision ([], 0)
concatRevisions (r:[]) = r
concatRevisions (Revision (es1,v1): Revision (es2,v2):rest) = concatRevisions (Revision (concatES es1 es2, v2):rest)
  where
    concatES :: [Edit] -> [Edit] -> [Edit]
    concatES (Preserve:ls) (Preserve:rs) = Preserve:concatES ls rs
    concatES (Preserve:ls) (Remove:rs) = (Remove:concatES ls rs)
    concatES l@(Preserve:ls) (Insert c:rs) = (Insert c:concatES l rs)

    concatES (Remove:ls) r = (Remove:concatES ls r)

    concatES (Insert c:ls) (Preserve:rs) = (Insert c:concatES ls rs)
    concatES (Insert c:ls) (Remove:rs) = concatES ls rs
    concatES l@(Insert _:ls) (Insert c:rs) = (Insert c:concatES l rs)
    concatES [] r = r


--a verziók növekvő sorrendben vannak!!!



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
commit :: HasDocumentHost m => String -> m Response
commit cdata = do
  liftIO $ putStrLn $ ("%%% commit " ++ cdata)
  case (parseRevision cdata) of
    Left msg  -> undefined --error msg
    Right rev -> do
      revs <- getRevisions
      --let revMVar = rc_revisions rc
      --revs <- liftIO $ takeMVar revMVar -- [Revision]
      --liftIO $ putStrLn $ "%%% clientRev: " ++ (show rev)
      --liftIO $ putStrLn $ "%%% oldRevs: " ++ (show revs)
      let new_rs = commit' rev revs
      setRevisions (fst new_rs)
--      liftIO $ putMVar revMVar (fst new_rs)
      --liftIO $ putStrLn $ "%%% newRevs: " ++ (show $ fst new_rs)
      return $ snd new_rs
        where
          commit' :: Revision -> [Revision] -> ([Revision], Response)
          commit' (Revision ([],v)) revs = (revs, checkoutOnly (after v revs))
            where
              checkoutOnly :: [Revision] -> Response
              checkoutOnly [] = NoChanges --CheckoutOnly (Revision ([], v))
              checkoutOnly rs = CheckoutOnly (concatRevisions rs)
          commit' r@(Revision (es,v)) revs
            | latestVersion revs == v = (revs ++ [r'],CommitSuccessful (v')) --commit
            | otherwise = (revs, CheckoutOnly (concatRevisions (after v revs)))
                where
                  r' = Revision (es, v')
                  v' = v+1
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
