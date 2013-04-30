--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- | Ez a modul végzi a konverziót a hálózaton utazó adatok és a
-- Haskell típusok között.
-- Alapvetően az Aeson könyvtárra épül, amely sablonok segítségével
-- automatikusan létrehozza a megfelelő átalakító függvényeket.

module DocSnap.Serialize
  ( deserialize
  , serialize
  ) where

import  Data.Text.Encoding (decodeUtf8, encodeUtf8)
import  DocSnap.Internal.Types
import  qualified Data.Text as T
import  qualified Data.Aeson as A
--import  qualified Data.Aeson.Generic as A
import  Data.Aeson.TH
import  qualified Data.ByteString as B
import  qualified Data.ByteString.Lazy as BL

-- | Válasz átalakítása hálózaton küldhető adatra
serialize :: A.ToJSON a => a -> B.ByteString
serialize = lbsToBs . A.encode 

-- | Válasz előállítása hálózaton érkezett adatból
deserialize :: A.FromJSON a => B.ByteString -> Maybe a
deserialize = A.decode . bsToLbs

-- | Konverziós rutinok
lbsToBs :: BL.ByteString -> B.ByteString
lbsToBs = B.pack . BL.unpack

bsToLbs :: B.ByteString -> BL.ByteString
bsToLbs = BL.pack . B.unpack

-- | JSON reprezentációk automatikus generálása
--todo: csak a szükségesek irányokat generáljuk
$(deriveJSON id ''PackedEdit)
$(deriveJSON id ''Revision)
$(deriveJSON id ''ChatMessage)
$(deriveJSON id ''UpdateResponse)
$(deriveJSON id ''Request)
$(deriveJSON (drop 14) ''ShareResponse)
$(deriveJSON (drop 13) ''ShareRequest)

test = Revision 10 [ I "proba", P 24, R 5]
test2 = Revision 10 []

