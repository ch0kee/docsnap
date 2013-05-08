--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- | A verziókezelő által használt típusok.

module DocSnap.Repository.Types  where

--------------------------------------------------------------------------------
import Data.Aeson.TH
import Control.Concurrent.MVar
import qualified Data.Text as T
import qualified Data.Map as M
import  Control.Monad.Trans
--------------------------------------------------------------------------------


type Content = String
type Length = Int
type Count = Int
type Version = Int


data SingleEdit = SI Char | SP | SR
  deriving (Show)
  
  
--------------------------------------------------------------------------------
-- | Repository
data Repository = Repository
    { documents :: MVar [MVar Document]
    , shares :: MVar ShareMap }

--------------------------------------------------------------------------------
-- | Revízió
data Revision = Revision
    { version::Version
    , edits::[PackedEdit] }
  deriving (Show)
  
--------------------------------------------------------------------------------
-- | Dokumentum
data Document = Document
    { revisions :: [Revision]
    , chatLog :: [(Int, ChatMessage)] }


--------------------------------------------------------------------------------
-- | Chat üzenet
data ChatMessage = ChatMessage
    { sender :: String
    , message :: String }
  deriving (Show)

--------------------------------------------------------------------------------
-- | Válasz objektum a frissítési kérelemre  
data UpdateResponse = UpdateResponse
    { rspRevision :: Revision
    , rspChatMessages :: [ChatMessage]
    , rspChatVersion :: Version }
  deriving (Show)
  
--------------------------------------------------------------------------------
-- | Frissítési kérelem típusa  
data Request = Request
    { reqRevision :: Revision
    , reqChatName :: String
    , reqChatBuffer :: [String]
    , reqChatVersion :: Version }
  deriving (Show)
  
--------------------------------------------------------------------------------
-- | Hozzáférési szint
data AccessRight = Author | Reader
  deriving (Eq)

--------------------------------------------------------------------------------
-- | Megosztási kérelem
data ShareRequest = ShareRequest
    { shareRequest_type :: String}

--------------------------------------------------------------------------------
-- | Megosztási link
data ShareResponse = ShareResponse 
    { shareResponse_link :: String }

type MDocument = MVar Document
type ShareMap = M.Map SharedKey DocumentAccess
type MShareMap = MVar ShareMap

--------------------------------------------------------------------------------
-- | Rögzített hozzáférési szintű dokumentum hozzáférés
newtype DocumentAccess = DocumentAccess (AccessRight, MDocument)  
  deriving (Eq)

--------------------------------------------------------------------------------
-- | Hozzáférés
data Access = Denied                  -- ^ megtagadott hozzáférés
            | Granted DocumentAccess  -- ^ engedélyezett hozzáférés


type ChatLog = [ChatMessage]


type SharedKey = T.Text

--------------------------------------------------------------------------------
-- | Segédosztály a Snaplet kényelmesebb használata céljából        
class (MonadIO m) => HasRepository m 
  where
    getRepository :: m Repository
    modifyRepository :: (Repository -> Repository) -> m ()
    

    
--------------------------------------------------------------------------------
{- nem használt definíciók, későbbi fejlesztéshez fenntartva 
compatible :: [PackedEdit] -> [PackedEdit] -> Bool
compatible = undefined -- True
-}

--------------------------------------------------------------------------------
-- | Szerkesztéslánc egy eleme
data PackedEdit =
    I String      -- ^ Beszúr <String>
  | P Int         -- ^ Helybenhagy n karaktert
  | R Int         -- ^ Töröl n karaktert
  deriving (Show)


{-
parallelMerge :: (Revision, Revision) -> (Revision, Revision)
parallelMerge (Revision v1 e1, Revision v2 e2) = undefined
-}

--------------------------------------------------------------------------------
-- | JSON reprezentációk automatikus generálása    
$(deriveJSON id ''PackedEdit)
$(deriveJSON id ''Revision)
$(deriveJSON id ''ChatMessage)
$(deriveJSON id ''UpdateResponse)
$(deriveJSON id ''Request)
$(deriveJSON (drop 14) ''ShareResponse)
$(deriveJSON (drop 13) ''ShareRequest)
