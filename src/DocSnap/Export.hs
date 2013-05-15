--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- | Minden, a Convertert megvalósító típuson keresztül exportálható
-- a dokumentum.
module DocSnap.Export
  ( runExport
  , exportersSplice
  , ExportRequest
  , ExportResponse
  ) where
  
--------------------------------------------------------------------------------
import  Data.Maybe (listToMaybe)
import qualified Text.XmlHtml as H (Node(..))
import Control.Monad.Trans (MonadIO)
import  qualified Data.Text as T 
import  Data.Aeson.TH
--------------------------------------------------------------------------------
import  DocSnap.Utilities (writeToRandomDir)
import  DocSnap.Splices (bulletListSplice, HasHeist, SnapletISplice)
import  DocSnap.Converter
import  DocSnap.Converter.Backends.TxtConverter
import  DocSnap.Converter.Backends.HtmlConverter
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Az aktív konverterek listája. Ide kell felvenni az újat, és automatikusan megjelenik 
-- a weboldalon, az 'exporter' menüben, mint lehetséges formátum.
converters :: [Converter]
converters = [MkConverter TxtConverter, MkConverter HtmlConverter]


--------------------------------------------------------------------------------
-- | Exportálási kérés futtatása. Amennyiben nem jár sikerrel,
-- egy üres linket küld vissza.
runExport :: ExportRequest     -- ^ export kérés
          -> T.Text            -- ^ kimeneti fájl neve
          -> String            -- ^ exportálandó tartalom
          -> IO ExportResponse -- ^ létrejött fájl
runExport (ExportRequest index) key content = do
    case listToMaybe (drop index converters) of
      Nothing        -> return $ ExportResponse ""
      Just (MkConverter converter) -> ExportResponse `fmap` writeToRandomDir (T.unpack key) "download" (render converter content)

--------------------------------------------------------------------------------
-- | Exportálási kérés, a hálózaton keresztül érkezik.        
newtype ExportRequest = ExportRequest
    { exportRequest_index :: Int }
    

--------------------------------------------------------------------------------
-- | Válasz az exportálási kérésre. Az url a létrejött fájlra mutat.
newtype ExportResponse = ExportResponse
    { exportResponse_url :: String }
    
--------------------------------------------------------------------------------
-- | Az export menü dinamikus felépítését generálja a weboldalhoz.
exportersSplice :: (HasHeist b) => SnapletISplice b
exportersSplice = bulletListSplice "exportmenu" converters
  (\(_, MkConverter conv) -> displayName conv)

--------------------------------------------------------------------------------
-- | JSON reprezentáció generálása.
$(deriveJSON (drop 14) ''ExportRequest)
$(deriveJSON (drop 15) ''ExportResponse)

