{-# LANGUAGE ExistentialQuantification #-}

--------------------------------------------------------------------------------
-- | Ebben a modulban találhatóak az elkészült tartalmak tetszőleges
-- formátumba történő kiexportálásához szükséges eszközök
module Exporter
  (
    -- * Típusosztály új exporterek létrehozásához
    ExportableFormat (..)
    -- * Konstruktor az exportálható formátumok dobozolásához
  , Exporter (MkExporter)
    -- * Fő exportáló függvény
  , exportToRandomFile
  ) where
  
--------------------------------------------------------------------------------
import           System.FilePath (FilePath, combine)
import           System.Directory (doesFileExist)
import           Data.Char (chr)
import           Control.Monad.Random (evalRandIO, getRandomRs)

--------------------------------------------------------------------------------
-- | Tartalom exportálása véletlen nevű fájlba.
exportToRandomFile :: String      -- ^ tartalom
                   -> Exporter    -- ^ exportáló
                   -> FilePath    -- ^ ebbe a könyvtárba kerül a kész fájl
                   -> IO FilePath -- ^ létrejött fájl elérési útvonala
exportToRandomFile cnt (MkExporter exp) dir = writeToRandomFile dir $ convert exp cnt

--------------------------------------------------------------------------------
-- | Csomagoló típus, hogy listában lehessen tárolni az exportereket (@boxing@)
data Exporter = forall a. ExportableFormat a => MkExporter a

--------------------------------------------------------------------------------
-- | Az exportálható típusosztály, ezt kell megvalósítania az exportereknek
class ExportableFormat a
  where
    displayName :: a -> String
    -- ^ ez fog megjelenni az export menüben az oldalon
    convert :: a -> String -> String
    -- ^ konvertáló rutin, ez végzi az átalakítást
    
--------------------------------------------------------------------------------
-- | ToDo: put this into UTILS
writeToRandomFile :: FilePath -> String -> IO FilePath
writeToRandomFile subdir content = do
    digits <- generateRandomDigits
    path <- searchUniqueFile subdir digits
    writeFile path content
    return path
  where
    searchUniqueFile :: FilePath -> FilePath -> IO FilePath
    searchUniqueFile subdir digits = do
        let randomPath = combine subdir $ take 10 digits
        exists <- doesFileExist randomPath
        if exists then searchUniqueFile subdir $ tail digits
                  else return randomPath
    generateRandomDigits :: IO [Char]
    generateRandomDigits = evalRandIO (getRandomRs (48,57)) >>= return . map chr

