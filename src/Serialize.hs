{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Serialize (
  parseRevision,
  parseResponse,
  parseRequest,
  serialize
) where


import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
--import qualified Text.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)
import    Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Internal.Types
import qualified Data.Text as T
import Data.Aeson
import qualified Data.Aeson.Generic as A
import Data.Aeson.TH
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Control.Monad (replicateM, mapM)
import Debug.Trace
-- [=14|+3:alm|-2]


test = Revision 10 [ I "proba", P 24, R 5]
test2 = Revision 10 []

lbsToBs :: BL.ByteString -> B.ByteString
lbsToBs = B.pack . BL.unpack

bsToLbs :: B.ByteString -> BL.ByteString
bsToLbs = BL.pack . B.unpack


$(deriveJSON id ''PackedEdit)
$(deriveJSON id ''Revision)

$(deriveJSON id ''ChatMessage)
$(deriveJSON id ''Response)

{-
--egyelőre 
toJPackedEdit :: PackedEdit -> JPackedEdit
toJPackedEdit (Inserts s) = I $ T.pack s 
toJPackedEdit (Removes n) = R n 
toJPackedEdit (Preserves n) = P n 

fromJPackedEdit :: JPackedEdit -> PackedEdit
fromJPackedEdit (I s) = Inserts $ T.unpack s 
fromJPackedEdit (R n) = Removes n 
fromJPackedEdit (P n) = Preserves n 

-}

serialize :: Response -> B.ByteString
serialize = lbsToBs . A.encode 

--serialize :: Revision -> String
--serialize = T.unpack . decodeUtf8 . lbsToBs . A.encode 

parseRevision :: B.ByteString -> Maybe Revision
parseRevision = A.decode . bsToLbs

parseResponse :: B.ByteString -> Maybe Response
parseResponse r = trace ("parseResponse:" ++ bsToStr r) $ A.decode . bsToLbs $ r

parseRequest :: B.ByteString -> Maybe Request
parseRequest r = trace ("parseRequest:" ++ bsToStr r) $ A.decode . bsToLbs $ r

bsToStr :: B.ByteString -> String
bsToStr =  T.unpack . decodeUtf8
