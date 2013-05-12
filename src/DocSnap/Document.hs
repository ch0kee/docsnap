module DocSnap.Document where

import DocSnap.AccessProvider
import DocSnap.VersionControl
import Data.UUID as UUID
import qualified Data.Text as T
import qualified Data.ByteString as B
import    Data.Text.Encoding (decodeUtf8, encodeUtf8)

--type Document = Repository
type DocumentVersionControl = VersionControl
data DocumentAccessLevel = Author | Reader
  deriving (Eq)
  
type DocumentAccess = Access DocumentAccessLevel Document
type DocumentAccessProvider = AccessProvider UUID DocumentAccessLevel Document
type SharedKey = UUID.UUID



decodeSharedKey :: B.ByteString -> Maybe SharedKey
decodeSharedKey = UUID.fromString . T.unpack . decodeUtf8

encodeSharedKey :: SharedKey -> T.Text
encodeSharedKey = T.pack . UUID.toString
