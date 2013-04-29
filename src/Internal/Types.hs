{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-} 
{-# LANGUAGE ExistentialQuantification #-}



module Internal.Types where

import qualified Data.Text as T
import Data.Data (Data)
import Data.Typeable (Typeable)
import qualified Data.Map as M
import  Control.Monad.Trans
import Control.Concurrent.MVar.Strict

import Control.DeepSeq
import Exporter

type Content = String
type Length = Int
type Count = Int


data SingleEdit = SI Char | SP | SR
  deriving (Show)

data PackedEdit =
    I String --insert str
  | P Int --preserve n
  | R Int --remove n
  deriving (Show, Data, Typeable)
           
data Revision = Revision
    { version::Version
    , edits::[PackedEdit] }
  deriving (Show, Data, Typeable)

type Version = Int

--átnevezni, ez valójában azt mondja meg, hogy
--aki küldei, melyik verzión van, és mivel van lemaradva
--az, akinek küldi
--newtype Revision = Revision ([PackedEdit], Version)
--  deriving (Show)


data ChatMessage = ChatMessage
    { sender :: String
    , message :: String }
  deriving (Show, Data, Typeable)
  
data Response = Response
    { rspRevision :: Revision
    , rspChatMessages :: [ChatMessage]
    , rspChatVersion :: Version }
  deriving (Show, Data, Typeable)
  
data Request = Request
    { reqRevision :: Revision
    , reqChatBuffer :: [String]
    , reqChatVersion :: Version }
  deriving (Show, Data, Typeable)
  
data AccessRight = Author | Reader
  deriving (Eq)
  
instance NFData AccessRight 

data InitialCheckout = InitialCheckout
    { initialContent :: T.Text }
  deriving (Data,Typeable,Show)

type MDocument = MVar Document
type ShareMap = M.Map SharedKey DocumentAccess
type MShareMap = MVar ShareMap

newtype DocumentAccess = DocumentAccess (AccessRight, MDocument)  
  deriving (Eq)

instance NFData DocumentAccess 
instance NFData (MVar Document)
instance NFData Document

data DocumentHost = DocumentHost
    { documents :: MVar [MDocument]
    , shares :: MVar ShareMap }


type ChatLog = [ChatMessage]

data Document = Document
    { revisions :: [Revision]
    , chatLog :: [(Int, ChatMessage)] }

type SharedKey = T.Text
type RevisionHistory = [Revision]

class MonadIO m => HasDocumentHost m 
  where
    getDocumentHost :: m DocumentHost
    modifyDH :: (DocumentHost -> DocumentHost) -> m ()
    



data HtmlFormat = HtmlFormat
instance ExportableFormat HtmlFormat
  where
    displayName = const "html file"
    convert _ = id

--data Exporter = forall a. ExportableFormat a => Exporter a





    
