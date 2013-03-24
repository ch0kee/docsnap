{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

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



{-
data DocSnapServer = DocSnapServer {
  dss_baseContent :: B.ByteString,
  dss_syncObject :: EditScript,
  dss_nextUpdate :: Map Contributor EditScript,
  dss_revisions :: [Revision]
}
-}

data Edit = Edit { edit_value :: String, edit_type :: String }
  deriving (Show) --atmenetileg

edit :: String -> String -> Edit
edit v t = Edit { edit_value=v, edit_type=t }

type EditScript = [Edit]

data Revision = Revision { rev_id :: String, rev_editscript :: EditScript  }
  deriving (Show)

data CommitData = CommitData { commitdata_editscript :: EditScript, commitdata_currentrevision :: String }

$(deriveJSON (drop 5) ''Edit)
$(deriveJSON (drop 11) ''Commit)
$(deriveJSON (drop 4) ''Revision)

type Contributor = String

newRevision id es = Revision { rev_id=c, rev_editscript=es }

--
--dss_init :: DocSnapServer
--dss_init = DocSnapServer { dss_baseContent = "", dss_syncObject = EditScript { so_diff = [] } }


data RevisionControl = RevisionControl {
  rc_revisions :: MVar [Revision]
}

revisionControlInit :: SnapletInit b RevisionControl
revisionControlInit = makeSnaplet "revctrl" "Revision Control snaplet" Nothing $ do
  mv <- liftIO $ newMVar []
  return RevisionControl { rc_revisions = mv }

class (MonadIO m) => HasRevisionControl m where
  getRevisionControlState :: m (RevisionControl)

getRevisions :: HasRevisionControl m => m [Revision]
getRevisions = do
  revctrl <- getRevisionControlState
  let revMVar = rc_revisions revctrl
  liftIO $ readMVar revMVar

{- flattenRevisions
osszeolvasztja egy revizioba a listat,
az id az utolso lesz
-}
flattenRevisions :: [Revision] -> Revision
flattenRevisions [] = error "there must be at least one revision"

type CharRevision = (Char, Char) --('A','+')

splitCharRev :: Revision -> [CharRevision]
splitCharRev Revision { rev_editscript=edits } = concat $ map splitToChars' edits
  where
    splitToChars' edit = map (,typ) val
      where
        typ = head $ edit_type edit
        val = edit_value edit

flattenRevisions r:[] = r
flattenRevisions r:r2:rest = fold zipWith merge (splitCharRev r1) (splitCharRev r2)
  where
    merge ((c,'-'):rest)              cr2       = (c,'-'): merge rest cr2
    merge ((c1,'+'):rest1) cr2@((c2,'+'):rest2) = (c1,'+'): merge rest1 cr2
    merge cr1             ((c,'+'):rest2)       = (c,'+'): merge cr1 rest2
    merge ((c,'+'):rest1)   ((_,'='):rest2)     = (c,'+'): merge rest1 rest2
    merge ((c,'+'):rest1)   ((_,'-'):rest2)     = merge rest1 rest2
    merge ((c,'='):rest1)        (c2:rest2)     = c2: merge rest1 rest2

--megkeressuk az aktualis reviziot
--azutantol kezdve mindent osszeolvasztunk,
--majd ezt kuldjuk vissza
commit :: HasRevisionControl m => CommitData -> m ()
commit cdata = do
  liftIO $ putStr "*commiting:"
  revctrl <- getRevisionControlState
  let revMVar = rc_revisions revctrl
  rs <- liftIO $ takeMVar revMVar
  let new_rs = newRevision c e:rs
  liftIO $ putStrLn (show new_rs)
  liftIO $ putMVar revMVar new_rs



