{-# LANGUAGE TemplateHaskell #-}

module Serialize (
  deserialize,
  serialize
) where

import  Data.Text.Encoding (decodeUtf8, encodeUtf8)
import  Internal.Types
import  qualified Data.Text as T
import  Data.Aeson
import  qualified Data.Aeson.Generic as A
import  Data.Aeson.TH
import  qualified Data.ByteString as B
import  qualified Data.ByteString.Lazy as BL

-- | Válasz átalakítása hálózaton küldhető adatra
serialize :: Response -> B.ByteString
serialize = lbsToBs . A.encode 

-- | Válasz előállítása hálózaton érkezett adatból
deserialize :: B.ByteString -> Maybe Request
deserialize = A.decode . bsToLbs

-- | Konverziós rutinok
lbsToBs :: BL.ByteString -> B.ByteString
lbsToBs = B.pack . BL.unpack

bsToLbs :: B.ByteString -> BL.ByteString
bsToLbs = BL.pack . B.unpack

-- | JSON reprezentációk automatikus generálása
$(deriveJSON id ''PackedEdit)
$(deriveJSON id ''Revision)
$(deriveJSON id ''ChatMessage)
$(deriveJSON id ''Response)


test = Revision 10 [ I "proba", P 24, R 5]
test2 = Revision 10 []

