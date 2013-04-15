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



data RevisionControl = RevisionControl {
  rc_revisions :: MVar [Revision]
}

revisionControlInit :: SnapletInit b RevisionControl
revisionControlInit = makeSnaplet "revctrl" "Revision Control snaplet" Nothing $ do
  mv <- liftIO $ newMVar []
  return RevisionControl { rc_revisions = mv }

class (MonadIO m) => HasRevisionControl m
  where
    getRevisionControlState :: m (RevisionControl)

--reviziok adott indextol
getRevisions :: HasRevisionControl m => m [Revision]
getRevisions = do
  revctrl <- getRevisionControlState
  let revMVar = rc_revisions revctrl
  liftIO $ readMVar revMVar



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


-- rak [+2:ab|=3] abrak
commit :: HasRevisionControl m => String -> m Response
commit cdata = do
  liftIO $ putStrLn $ ("%%% commit " ++ cdata)
  case (parseRevision cdata) of
    Left msg  -> undefined --error msg
    Right rev -> do
      rc <- getRevisionControlState
      let revMVar = rc_revisions rc
      revs <- liftIO $ takeMVar revMVar -- [Revision]
      --liftIO $ putStrLn $ "%%% clientRev: " ++ (show rev)
      --liftIO $ putStrLn $ "%%% oldRevs: " ++ (show revs)
      let new_rs = commit' rev revs
      liftIO $ putMVar revMVar (fst new_rs)
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
