{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
data Edit = Insert Char | Preserve | Remove
  deriving (Show)

type Version = Int
newtype Revision = Revision ([Edit], Version)
  deriving (Show)

data PackedEdit = Inserts String | Preserves Count | Removes Count
  deriving (Show)
newtype PackedRevision = PackedRevision ([PackedEdit], Version)
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
, access :: Maybe AuthorAccess
}

data Author
data Reader

data AuthorAccess = AuthorAccess { key :: SharedKey, doc :: TDocument }

type SharedKey = String
type DocumentRef = Integer
type RevisionHistory = [Revision]
newtype Document = Document { revisions :: RevisionHistory }

class MonadIO m => HasDocumentHost m 
  where
    getDocumentHost :: m DocumentHost
    modifyDH :: (DocumentHost -> DocumentHost) -> m ()
