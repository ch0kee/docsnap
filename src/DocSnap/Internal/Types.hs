{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE ExistentialQuantification #-}



module DocSnap.Internal.Types where

import qualified Data.Text as T
import qualified Data.Map as M
import  Control.Monad.Trans
import Control.Concurrent.MVar


type Content = String
type Length = Int
type Count = Int


data SingleEdit = SI Char | SP | SR
  deriving (Show)

data PackedEdit =
    I String --insert str
  | P Int --preserve n
  | R Int --remove n
  deriving (Show)
           
data Revision = Revision
    { version::Version
    , edits::[PackedEdit] }
  deriving (Show)

type Version = Int

--átnevezni, ez valójában azt mondja meg, hogy
--aki küldei, melyik verzión van, és mivel van lemaradva
--az, akinek küldi
--newtype Revision = Revision ([PackedEdit], Version)
--  deriving (Show)


data ChatMessage = ChatMessage
    { sender :: String
    , message :: String }
  deriving (Show)
  
data UpdateResponse = UpdateResponse
    { rspRevision :: Revision
    , rspChatMessages :: [ChatMessage]
    , rspChatVersion :: Version }
  deriving (Show)
  
  {-
data CleanContentResponse = UpdateResponse
    { rspRevision :: Revision
    , rspChatMessages :: [ChatMessage]
    , rspChatVersion :: Version }
  deriving (Show, Data, Typeable)
  -}
  
data Request = Request
    { reqRevision :: Revision
    , reqChatName :: String
    , reqChatBuffer :: [String]
    , reqChatVersion :: Version }
  deriving (Show)
  
data AccessRight = Author | Reader
  deriving (Eq)

data ShareRequest = ShareRequest
    { shareRequest_type :: String}

data ShareResponse = ShareResponse 
    { shareResponse_link :: String }
    



data InitialCheckout = InitialCheckout
    { initialContent :: T.Text }
  deriving (Show)

type MDocument = MVar Document
type ShareMap = M.Map SharedKey DocumentAccess
type MShareMap = MVar ShareMap

newtype DocumentAccess = DocumentAccess (AccessRight, MDocument)  
  deriving (Eq)


data Repository = Repository
    { documents :: MVar [MDocument]
    , shares :: MVar ShareMap }


type ChatLog = [ChatMessage]

data Document = Document
    { revisions :: [Revision]
    , chatLog :: [(Int, ChatMessage)] }

type SharedKey = T.Text
type RevisionHistory = [Revision]

class (MonadIO m) => HasRepository m 
  where
    getRepository :: m Repository
    modifyRepository :: (Repository -> Repository) -> m ()
    

data Access = Denied | Granted DocumentAccess





    
