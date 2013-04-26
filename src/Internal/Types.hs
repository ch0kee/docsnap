{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Internal.Types where

import qualified Data.Text as T
import Control.Concurrent.STM
import Data.Data (Data)
import Data.Typeable (Typeable)
import qualified Data.Map as M
import  Control.Monad.Trans
import Control.Concurrent.MVar

type Content = String
type Length = Int
type Count = Int
{-
data PackedEdit = Inserts String | Preserves Count | Removes Count
  deriving (Show)
-}

data SingleEdit = SI Char | SP | SR
  deriving (Show)

data PackedEdit = I String --insert str
           | P Int --preserve n
           | R Int --remove n
  deriving (Show, Data, Typeable)
           
data Revision = Revision { version::Version, edits::[PackedEdit] }
  deriving (Show, Data, Typeable)

type Version = Int

--átnevezni, ez valójában azt mondja meg, hogy
--aki küldei, melyik verzión van, és mivel van lemaradva
--az, akinek küldi
--newtype Revision = Revision ([PackedEdit], Version)
--  deriving (Show)


data JChatMessage = JChatMessage { sender :: String, message :: String }
  deriving (Show, Data, Typeable)
data Response  = Response { sessionId :: SessionId, revision :: Revision}
  deriving (Show, Data, Typeable)
  
data AccessRight = Author | Reader
  deriving (Eq)
  
data InitialCheckout = InitialCheckout { content :: T.Text }
  deriving (Data,Typeable,Show)

type MDocument = MVar Document
type ShareMap = M.Map SharedKey DocumentAccess
type MShareMap = MVar ShareMap

newtype DocumentAccess = DocumentAccess (AccessRight, MDocument)  
  deriving (Eq)

data DocumentHost = DocumentHost {
  documents :: MVar [MDocument]
, shares :: MVar ShareMap
}

type SessionId = Int
data Editor = Editor {
  sessId :: SessionId
, name :: String
, touched :: Bool
}

data Document = Document {
  revisions :: [Revision]
, editors :: [Editor]
}

type SharedKey = T.Text
type RevisionHistory = [Revision]

class MonadIO m => HasDocumentHost m 
  where
    getDocumentHost :: m DocumentHost
    modifyDH :: (DocumentHost -> DocumentHost) -> m ()
    
    
