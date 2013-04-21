{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}

module DocSnapServer where


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
import qualified Control.Monad.State as S
import Control.Monad (liftM, liftM2, ap)
import Control.Applicative
import qualified Data.Text as T

import Data.UUID as UUID
import Control.Monad.Random --for random in STM
--we can use tvar for the map, since
--we dont expect document-create-overflow

--todo: we use parametrized types to lift side effects into user code
--by using 'a' instead of 'MVar'
import Internal.Types






tryAccessAsAuthor :: HasDocumentHost m => SharedKey -> m (Maybe AuthorAccess)
tryAccessAsAuthor sk = do
  liftIO $ putStrLn ("try access with sk=" ++ sk) 
  tm <- liftM authorShareMap getDocumentHost
  dm <- liftIO . readTVarIO $ tm 
  let acc = M.lookup sk dm >>= (\d -> Just $ AuthorAccess sk d)
  modifyDH (\d -> d {access=acc})
  return acc

emptyDocument :: Document
emptyDocument = Document [] 

documentHostInit :: SnapletInit b DocumentHost
documentHostInit = makeSnaplet "dochost" "DocumentHost Snaplet" Nothing $ do
  (asm,rsm) <- liftIO . atomically $ liftM2 (,) (newTVar M.empty) (newTVar M.empty)
  return $ DocumentHost asm rsm Nothing

--generateSharedKey :: IO (SharedKey)
--generateSharedKey = UUID.nextRandom >> UUID.toString

--ez inkabb egy olyan monadban ugykodjon, ahol biztos nem csesztet mast
createDocument :: HasDocumentHost m => m SharedKey
createDocument = do
  tm <- getDocumentHost >>= return . authorShareMap
  liftIO $ do
    gen <- newStdGen
    newdoc <- newTVarIO emptyDocument
    atomically $ evalRandT (insetWithUniqueKey newdoc tm) gen
    
  where
    insetWithUniqueKey :: TDocument -> TShareMap -> RandT StdGen STM SharedKey
    insetWithUniqueKey newdoc tm = do
      uuid <- getRandom
      let sk = UUID.toString uuid
      m <- lift $ readTVar tm
      newdoc <- lift $ newTVar emptyDocument
      lift $ case M.lookup sk m of --in case of 'wonders'
        Just _ -> retry
        _      -> writeTVar tm (M.insert sk newdoc m) 
      return sk
    

getRevisions :: HasDocumentHost m => AuthorAccess -> m [Revision]
getRevisions a = liftM revisions $ liftIO . readTVarIO . doc $ a


--WARNING
appendRevision :: HasDocumentHost m => Revision -> AuthorAccess -> m ()
appendRevision r acc = do
  liftIO . atomically $ modifyTVar' (doc acc) (\d -> Document $ (revisions d ++ [r]))

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
commit :: HasDocumentHost m => String -> AuthorAccess -> m Response
commit cdata acc = do
  liftIO $ putStrLn $ ("%%% commit " ++ cdata)
  case parseRevision cdata of
    Left msg  -> undefined --error msg
    Right rev -> do
      revs <- getRevisions acc
      --let revMVar = rc_revisions rc
      --revs <- liftIO $ takeMVar revMVar -- [Revision]
      --liftIO $ putStrLn $ "%%% clientRev: " ++ (show rev)
      --liftIO $ putStrLn $ "%%% oldRevs: " ++ (show revs)
      let new_rs = commit' rev revs
      case fst new_rs of
        Nothing -> return ()
        Just r  -> appendRevision r acc
--      setRevisions )
--      liftIO $ putMVar revMVar (fst new_rs)
      --liftIO $ putStrLn $ "%%% newRevs: " ++ (show $ fst new_rs)
      return $ snd new_rs
        where
          commit' :: Revision -> [Revision] -> (Maybe Revision, Response)
          commit' (Revision ([],v)) revs = (Nothing, checkoutOnly (after v revs))
            where
              checkoutOnly :: [Revision] -> Response
              checkoutOnly [] = NoChanges --CheckoutOnly (Revision ([], v))
              checkoutOnly rs = CheckoutOnly (concatRevisions rs)
          commit' r@(Revision (es,v)) revs
            | latestVersion revs == v = (Just r',CommitSuccessful (v')) --commit
            | otherwise = (Nothing, CheckoutOnly (concatRevisions (after v revs)))
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

