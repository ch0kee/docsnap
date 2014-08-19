--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- | VersionControl modul. Egy verziókezelőt implementál.
module DocSnap.VersionControl
  ( update
  , chat
  , VersionControl (..)
  , Document (..)
  , createDocument
  , rawContent
  , initVersionControl
  ) where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar
import Control.Monad.Trans
import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import           Snap.Snaplet
import           Control.Monad.State 
import DocSnap.VersionControl.Merge



data VersionControl = VersionControl
  { documents :: MVar [Document] }

data Document = Document
  { revisions :: MVar [Revision]
  , chatlog :: MVar [ChatMessage] }
  deriving (Eq)
  
type Version = Int

data Revision = Revision
  { version :: Version
  , editScript :: [Edit] }
    deriving(Show)

--------------------------------------------------------------------------------
-- | Verziókezelő snaplet inicializálása.
initVersionControl :: SnapletInit b (VersionControl)
initVersionControl = makeSnaplet "vc" "Version Control Snaplet" Nothing $ do
    docs <- liftIO $ newMVar []
    return $ VersionControl docs

-- | a dokumentum nyers, legfrissebb verzió szerinti szövegét adja vissza
rawContent :: (MonadIO m)
         => Document
         -> m String
rawContent doc = do
    revs <- liftIO $ readMVar $ revisions doc
    return $  concat . map extract $ foldl compose [] $ map editScript revs
      where
        extract (I str) = str
        extract _       = []

--------------------------------------------------------------------------------
-- | Új repository létrehozása.
createDocument :: Handler b VersionControl Document
createDocument = do
    mdocs <- gets documents
    liftIO $ modifyMVar mdocs $ \docs -> do
        d <- emptyDocument
        return (d:docs, d)
  where
    emptyDocument :: IO Document
    emptyDocument = liftM2 Document (newMVar [ Revision 0 [I welcomeString] ]) (newMVar [])
    -- | Ez a szöveg van kezdetben a szerkesztőben
    welcomeString :: String
    welcomeString = "Welcome to DocSnap!\nYou can now start editing this text."
    
    
--------------------------------------------------------------------------------
-- | Revíziók rögzítése,


update :: (MonadIO m)
       => Document
       -> Revision
       -> m Revision
update doc (Revision cliVersion []) = do
    revs <- liftIO . readMVar $ revisions doc
    let srvVersion = latestVersion revs
    let srvEdits = foldl compose [] $ map editScript $ after cliVersion revs
    return $ Revision srvVersion srvEdits

update doc (Revision cliVersion cliEdits) = do
    liftIO $ modifyMVar (revisions doc) $ \revs -> do
        let srvVersion = latestVersion revs
        let srvEdits = foldl compose [] $ map editScript $ after cliVersion revs
        let (p1,p2) = merge (srvEdits, cliEdits)
        return (revs ++ [Revision (srvVersion+1) p1], Revision (srvVersion+1) p2)

-- | Chat üzenetek cseréje 
chat :: (MonadIO m)
       => Document
       -> ChatSyncData
       -> m ChatSyncData
chat doc (ChatSyncData cliVersion messages) = do
    liftIO . modifyMVar (chatlog doc) $ \chatlog -> do
        let newchatlog = chatlog ++ messages
        let sendBack = drop cliVersion newchatlog
        return (newchatlog, ChatSyncData (length newchatlog) sendBack)



   --------------------------------------------------------------------------------
-- | A legfrissebb verziószám
latestVersion :: [Revision] -> Version
latestVersion [] = 0
latestVersion rs = version (last rs)


--------------------------------------------------------------------------------
-- | Változások csak egy adott verzió után
after :: Version -> [Revision] -> [Revision]
after v rs = dropWhile (\r -> (version r) <= v) rs


-- | Chat üzenet
data ChatMessage = ChatMessage
    { cm_sender :: String
    , cm_message :: String }
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
data ChatSyncData = ChatSyncData
    { csd_version :: Version
    , csd_messages :: [ChatMessage] }
  deriving(Show)

    
$(deriveJSON defaultOptions ''Edit)
$(deriveJSON defaultOptions ''Revision)
$(deriveJSON defaultOptions{fieldLabelModifier=drop 3} ''ChatMessage)
$(deriveJSON defaultOptions ''UpdateResponse)
$(deriveJSON defaultOptions ''Request)
$(deriveJSON defaultOptions{fieldLabelModifier=drop 4} ''ChatSyncData)

  
  
