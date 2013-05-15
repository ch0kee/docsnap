--------------------------------------------------------------------------------
{-# LANGUAGE GADTs #-}
--------------------------------------------------------------------------------
-- | az IConverter interfészt megvalósító típus az elkészült tartalmat tetszőleges
-- formátumba konvertálhatja
module DocSnap.Converter where


--------------------------------------------------------------------------------
-- | Csomagoló típus, hogy listában lehessen tárolni az exportereket (@boxing@)
-- * Konstruktor az exportálható formátumok dobozolásához
--data Converter = forall a. IConverter a => Converter a

data Converter where
  MkConverter :: IConverter a => a -> Converter

--------------------------------------------------------------------------------
-- | Az exportálható típusosztály, ezt kell megvalósítania az exportereknek
-- * Típusosztály új exporterek létrehozásához
class IConverter a
  where
    displayName :: a -> String
    -- ^ ez fog megjelenni az export menüben az oldalon
    render :: a -> String -> String
    -- ^ konvertáló rutin, ez végzi az átalakítást

