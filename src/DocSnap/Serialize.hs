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
import  qualified Data.Text as T
import  qualified Data.Aeson as A
--import  qualified Data.Aeson.Generic as A
import  Data.Aeson.TH
import  qualified Data.ByteString as B
import  qualified Data.ByteString.Lazy as BL

-- | Válasz átalakítása hálózaton küldhető adatra
serialize :: (A.ToJSON a)
          => a              -- ^ Haskell érték
          -> B.ByteString   -- ^ szerializált Haskell érték
serialize = lbsToBs . A.encode 

-- | Válasz előállítása hálózaton érkezett adatból
deserialize :: (A.FromJSON a)
            => B.ByteString -- ^ hálózaton érkező bájtfűzér
            -> Maybe a      -- ^ deszerializált Haskell érték
deserialize = A.decode . bsToLbs

-- | Konverziós rutinok
lbsToBs :: BL.ByteString -> B.ByteString
lbsToBs = B.pack . BL.unpack

bsToLbs :: B.ByteString -> BL.ByteString
bsToLbs = BL.pack . B.unpack

-- | JSON reprezentációk automatikus generálása



