-- | A dokumentumok kezeléséhez szükséges, a verziókezelésen kívüli további típusok és függvények
module DocSnap.Document
  ( DocumentVersionControl (..)
  , DocumentAccessProvider (..)
  , DocumentAccessLevel (..)
  , DocumentAccess(..)
  , SharedKey
  , decodeSharedKey
  , encodeSharedKey
  , sharedKeyToString
  ) where

import DocSnap.AccessProvider
import DocSnap.VersionControl
import qualified Data.Text as T
import qualified Data.ByteString as B
import    Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Random

type DocumentVersionControl = VersionControl
data DocumentAccessLevel = Author | Reader
  deriving (Eq)
  
type DocumentAccess = Access DocumentAccessLevel Document
type DocumentAccessProvider = AccessProvider DocumentIdentifier DocumentAccessLevel Document
type SharedKey = DocumentIdentifier


-- | Konverziós rutinok
decodeSharedKey :: B.ByteString -> Maybe SharedKey
decodeSharedKey = tryDecode . T.unpack . decodeUtf8
  where
    tryDecode s | valid s = Just $ DocumentIdentifier s 
    tryDecode _ = Nothing 
    valid s = (length s == documentIdLength) && (all (`elem` documentIdChars) s)

encodeSharedKey :: SharedKey -> T.Text
encodeSharedKey = T.pack . documentIdentifier

sharedKeyToString :: SharedKey -> String
sharedKeyToString = documentIdentifier

-- | dokumentum azonosító
newtype DocumentIdentifier = DocumentIdentifier {
  documentIdentifier :: String
}
  deriving(Ord, Eq)

-- | dokumentum azonosító szabályos karakterei
documentIdChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

-- | dokumentum azonosító hossza
documentIdLength = 8

-- | véletlenszerű kulcs generálása
instance Random DocumentIdentifier
  where
    randomR (a,b) g = undefined -- | nem használjuk
    random g = (\(s, g') -> (DocumentIdentifier s, g')) $ foldl randomDigit ("", g) [1..documentIdLength]
      where
        randomDigit (rest, g') _ = (\(c, g'') -> (c:rest, g'') ) $ randomChar g'
        randomChar g' = indexToChar $ randomR (0, (length documentIdChars) -1)  g'
        indexToChar (i,g') = (documentIdChars !! i, g')

