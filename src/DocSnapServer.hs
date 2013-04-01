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


{-
data DocSnapServer = DocSnapServer {
  dss_baseContent :: B.ByteString,
  dss_syncObject :: EditScript,
  dss_nextUpdate :: Map Contributor EditScript,
  dss_revisions :: [Revision]
}
-}

{-
data EditJSON = EditJSON { ej_value :: String, ej_size :: Int, ej_type :: String }
  deriving (Show) --atmenetileg
data RevisionJSON = RevisionJSON { rj_version :: Int, rj_editscript :: [EditJSON] }
  deriving (Show)
data CheckoutDataJSON = CheckoutDataJSON {
  cdj_revision :: RevisionJSON,
  cdj_checkout :: Bool }


class (A.ToJSON b) => Serializable a b
  where
    toSerializable  :: a -> b
    fromSerializable :: b -> a

--4!!!!
instance Serializable Edit EditJSON
  where
    toSerializable :: Edit -> EditJSON
    toSerializable (Add c) = EditJSON (c:[]) 1 "+"
    toSerializable Equal = EditJSON "" 1 "="
    toSerializable Remove = EditJSON "" 1 "-"

    fromSerializable :: EditJSON -> Edit
    fromSerializable (EditJSON v s "+") = map Add v
    fromSerializable (EditJSON v s "-") = replicate s Remove
    fromSerializable (EditJSON v s "=") = replicate s Equal

instance Serializable Revision RevisionJSON
  where
    toSerializable :: Revision -> RevisionJSON
    toSerializable (Revision (es, v)) = RevisionJSON v (map toSerializable es)

    fromSerializable :: RevisionJSON -> Revision
    fromSerializable (RevisionJSON v es) = Revision (map fromSerializable es)


serialize (A.ToJSON a) => a -> B.ByteString
serialize = B.pack . BL.unpack . A.encode

deserialize (A.FromJSON a) => B.ByteString -> a
deserialize = A.decode . BL.pack . B.unpack


$(deriveJSON (drop 3) ''EditJSON)
-- $(deriveJSON (drop 11) ''Commit)
$(deriveJSON (drop 3) ''RevisionJSON)
$(deriveJSON (drop 4) ''CheckoutDataJSON)
-}

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

{-
editJSONtoEdit :: EditJSON -> [Edit]
editJSONtoEdit (EditJSON { edit_type=t, edit_value=v })
  | t == "+" = map Add v
  | t == "-" = replicate (length v) Remove
  | t == "=" = replicate (length v) Equal
-}


--egymas utan kovetkezo reviziok osszefuzese egybe
concatRevisions :: [Revision] -> Revision
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
    concatES (Insert c:ls) r = (Insert c:concatES ls r)

{-
fold zipWith merge (splitCharRev r1) (splitCharRev r2)
  where
    merge ((c,'-'):rest)              cr2        = (c,'-'): merge rest cr2
    merge ((c1,'+'):rest1)  cr2@((c2,'+'):rest2) = (c1,'+'): merge rest1 cr2
    merge cr1               ((c,'+'):rest2)      = (c,'+'): merge cr1 rest2
    merge ((c,'+'):rest1)   ((_,'='):rest2)      = (c,'+'): merge rest1 rest2
    merge ((c,'+'):rest1)   ((_,'-'):rest2)      = merge rest1 rest2
    merge ((c,'='):rest1)        (c2:rest2)      = c2: merge rest1 rest2
-}

--if commit revision equals latest revision
----append commit revision to history
----send back new version
--else
----interrupt commit
----send back Revision ([intermediate revisions], latest version)

--a verziók növekvő sorrendben vannak!!!

latestVersion :: [Revision] -> Version
latestVersion [] = 0
latestVersion rs = (\(Revision (es,v)) -> v) (last rs)

un_revision (Revision (es,v)) = (es,v)

nextVersion :: [Revision] -> Version
nextVersion = (+1) . latestVersion

--változások adott verzió után
after :: Version -> [Revision] -> [Revision]
after v rs = dropWhile (\r -> (snd $ un_revision r) < v) rs


-- rak [+2:ab|=3] abrak
commit :: HasRevisionControl m => String -> m Response
commit cdata = do
  liftIO $ putStr "** committing"
  case (parseRevision cdata) of
    Left msg  -> undefined --error msg
    Right rev -> do
      rc <- getRevisionControlState
      let revMVar = rc_revisions rc
      revs <- liftIO $ takeMVar revMVar -- [Revision]
      let new_rs = commit' rev revs
      liftIO $ putMVar revMVar (fst new_rs)
      return $ snd new_rs
        where
          commit' :: Revision -> [Revision] -> ([Revision], Response)
          commit' (Revision ([],v)) revs = (revs, checkoutOnly (after v revs))
            where
              checkoutOnly :: [Revision] -> Response
              checkoutOnly [] = CheckoutOnly (Revision ([], v))
              checkoutOnly rs = CheckoutOnly (concatRevisions rs)
          commit' r@(Revision (es,v)) revs
            | latestVersion revs == v = (revs ++ [r],CommitSuccessful (v+1)) --commit
            | otherwise = (revs, CheckoutOnly (concatRevisions (after v revs)))




