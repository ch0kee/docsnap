--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module DocSnap.VersionControl where

import Control.Concurrent.MVar
import Control.Monad.Trans
import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Debug.Trace

import           Snap.Snaplet
import Control.Monad.State 
import DocSnap.Merge

data VersionControl = VersionControl
  { repositories :: MVar [Repository] }

data Repository = Repository
  { revisions :: MVar [Revision] }
  deriving (Eq)
  
type Version = Int

data Revision = Revision
  { version :: Version
  , editScript :: [Edit] }
    deriving(Show)



rawContent :: (MonadIO m)
         => Repository
         -> m String
rawContent repo = do
    revs <- liftIO $ readMVar $ revisions repo
    return $ maybe [] (concat . map extract . editScript) $ seqMergeRevisions revs
      where
        extract (I str) = str
        extract _       = []

checkout :: (MonadIO m)
         => Repository
         -> m Revision
checkout repo = do
    revs <- liftIO $ readMVar $ revisions repo
    return $ maybe (Revision 0 []) id $ seqMergeRevisions revs
--------------------------------------------------------------------------------
-- | Új repository létrehozása.
createRepository :: Handler b VersionControl Repository
createRepository = do
    mreps <- gets repositories
    liftIO $ modifyMVar mreps $ \reps -> do
        r <- emptyRepository
        return (r:reps, r)
  where
    emptyRepository :: IO (Repository)
    emptyRepository = liftM Repository $ newMVar [ Revision 0 [] ]
    
--------------------------------------------------------------------------------
-- | Revíziók rögzítése,
-- revíziókat.
{-
update :: (MonadIO m)
       => Repository
       -> Revision
       -> m Revision
update repo (Revision cliVersion cliEdits) = do
  cliResp <- liftIO $ modifyMVar (revisions repo) $ \revs -> do
      let srvEdits = maybe seqMergeRevisions $ after cliVersion revs
      if null cliEdits then 
         then return (revs, maybe (Revision cliVersion []) id retRev)
      (srvEdits',cliEdits') = merge (srvEdits, cliEdits)
      return (revs ++ srvEdits', cliEdits')
  
      case cliEdits of
        [] -> trace "checkout" $ do
            let retRev = seqMergeRevisions $ after cliVersion revs
            return (revs, maybe (Revision cliVersion []) id retRev)
        _         | latestVersion revs == cliVersion -> trace "commit" $ do
                      return ( revs ++ [(Revision (cliVersion+1) cliEdits)]
                             , Revision (cliVersion+1) [])
                  | otherwise -> trace "late. checkout. " $ do
                      let retRev = seqMergeRevisions $ after cliVersion revs
                      return (revs, maybe (Revision cliVersion []) id retRev)
  trace ("send back "++(show cliResp)) $ return cliResp

-}

update :: (MonadIO m)
       => Repository
       -> Revision
       -> m Revision
update repo (Revision cliVersion cliEdits) = do
  cliResp <- liftIO $ modifyMVar (revisions repo) $ \revs -> do
      let srvVersion = latestVersion revs
      let srvEdits = foldl compose [] $ map editScript $ after cliVersion revs
      case cliEdits of
        [] -> do
                return (revs, Revision srvVersion srvEdits)
        _  -> do
                if srvVersion == cliVersion
                    then return (revs ++ [Revision (srvVersion+1) cliEdits], Revision (srvVersion+1) [])
                    else do
                      let (p1,p2) = merge (srvEdits, cliEdits)
                      return (revs ++ [Revision (srvVersion+1) p1], Revision (srvVersion+1) p2)
  return cliResp
{-
update :: (MonadIO m)
       => Repository
       -> Revision
       -> m Revision
update repo (Revision cliVersion cliEdits) = do
  cliResp <- liftIO $ modifyMVar (revisions repo) $ \revs -> do
      let srvVersion = latestVersion revs
      let srvEdits = foldl compose [] $ map editScript $ after cliVersion revs
      case cliEdits of
        [] -> trace "checkout" $ do
                return (revs, Revision srvVersion srvEdits)
        _  -> trace "commit" $ do
                if srvVersion == cliVersion
                    then return (revs ++ [Revision (srvVersion+1) cliEdits], Revision (srvVersion+1) [])
                    else return (revs, (Revision srvVersion srvEdits))
  trace ("send back "++(show cliResp)) $ return cliResp
-}
   --------------------------------------------------------------------------------
-- | A legfrissebb verziószám
latestVersion :: [Revision] -> Version
latestVersion [] = 0
latestVersion rs = version (last rs)


--------------------------------------------------------------------------------
-- | Változások csak egy adott verzió után
after :: Version -> [Revision] -> [Revision]
after v rs = dropWhile (\r -> (version r) <= v) rs

data SingleEdit = SI Char | SP | SR
  deriving (Show) 
--------------------------------------------------------------------------------
-- | Revíziók szekvenciális összefűzése. Üres lista esetén
-- Nothing-gal tér vissza, egyébként Just <összefűzött revízió>-val
seqMergeRevisions :: [Revision]     -- ^ összefűzendő revíziók
                  -> Maybe (Revision)-- ^ összefűzött revízió
seqMergeRevisions [] = Nothing
seqMergeRevisions [r] = Just r
seqMergeRevisions revs = Just $ Revision {
        version=(version . last) revs
--      , editScript=packEdit . foldl seqMergeEdits [] . map (unpackEdit . editScript) $ revs
      , editScript=foldl compose [] . map editScript $ revs
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
    unpackEdit (I []:rest) = unpackEdit rest 
    unpackEdit (I (c:str):rest) = SI c: unpackEdit (I str:rest)
    unpackEdit [] = []
    
    packEdit :: [SingleEdit] -> [PackedEdit]
    packEdit es = reverse $ snd $ packEdit' (es, [])
    packEdit' (SR:ur, R n:pr) = packEdit' (ur, R (n+1):pr)
    packEdit' (SR:ur, p) = packEdit' (ur, R 1:p)
    
    packEdit' (SP:ur, P n:pr) = packEdit' (ur, P (n+1):pr)
    packEdit' (SP:ur, p) = packEdit' (ur, P 1:p)
    
    packEdit' (SI c:ur, I s:pr) = packEdit' (ur, I (s++[c]):pr)
    packEdit' (SI c:ur, p) = packEdit' (ur, I (c:[]):p)  
    
    packEdit' ([], p) = ([], p)
  
  
    

  
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
    
    --------------------------------------------------------------------------------
-- | Válasz objektum a frissítési kérelemre  
data UpdateResponse = UpdateResponse
    { rspRevision :: Revision
    , rspChatMessages :: [ChatMessage]
    , rspChatVersion :: Version }
  deriving(Show)

  
--------------------------------------------------------------------------------
-- | Frissítési kérelem típusa  
data Request = Request
    { reqRevision :: Revision
    , reqChatBuffer :: [String]
    , reqChatName :: String
    , reqChatVersion :: Version }
  deriving(Show)
  
  -- | Chat üzenet
data ChatMessage = ChatMessage
    { sender :: String
    , message :: String }
  deriving(Show)

    
$(deriveJSON id ''Edit)
$(deriveJSON id ''Revision)
$(deriveJSON id ''ChatMessage)
$(deriveJSON id ''UpdateResponse)
$(deriveJSON id ''Request)
$(deriveJSON (drop 14) ''ShareResponse)
$(deriveJSON (drop 13) ''ShareRequest)
  
  
