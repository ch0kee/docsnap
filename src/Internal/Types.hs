{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Internal.Types where

import qualified Data.Text as T
import Control.Concurrent.STM
import Data.Data (Data)
import Data.Typeable (Typeable)
import qualified Data.Map as M
import  Control.Monad.Trans

type Content = String
type Length = Int
type Count = Int

data PackedEdit = Inserts String | Preserves Count | Removes Count
  deriving (Show)

data SingleEdit = Insert Char | Preserve | Remove
  deriving (Show)

data JPackedEdit = I T.Text
           | P Int
           | R Int
  deriving (Show, Data, Typeable)
           
data JRevision = JRevision { version::Version, edits::[JPackedEdit] }
  deriving (Show, Data, Typeable)

type Version = Int

--átnevezni, ez valójában azt mondja meg, hogy
--aki küldei, melyik verzión van, és mivel van lemaradva
--az, akinek küldi
newtype Revision = Revision ([PackedEdit], Version)
  deriving (Show)


data Response  = CommitSuccessful Version
               | CheckoutOnly Revision
               | NoChanges
  deriving (Show)
  

  
data InitialCheckout = InitialCheckout { content :: T.Text }
  deriving (Data,Typeable,Show)

type TDocument = TVar Document
type TShareMap = TVar (M.Map SharedKey TDocument)
data DocumentHost = DocumentHost {
  authorShareMap :: TShareMap
, readerShareMap :: TShareMap
}

data Author
data Reader

data AuthorAccess = AuthorAccess { key :: SharedKey, doc :: TDocument }

type SharedKey = T.Text
type DocumentRef = Integer
type RevisionHistory = [Revision]
newtype Document = Document { revisions :: RevisionHistory }

class MonadIO m => HasDocumentHost m 
  where
    getDocumentHost :: m DocumentHost
    modifyDH :: (DocumentHost -> DocumentHost) -> m ()
    
    
