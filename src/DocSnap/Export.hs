--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- | Minden, az IExporter osztályt megvalósító típusEbben a modulban találhatóak
-- az elkészült tartalmak tetszőleges
-- formátumba történő kiexportálásához végző típusok
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
import  DocSnap.Internal.Utilities (writeToRandomFile)
import  DocSnap.Snap.Splices (bulletListSplice, HasHeist, SnapletISplice)
import  DocSnap.Export.Converter
import  DocSnap.Export.Converter.Backends.TxtConverter
import  DocSnap.Export.Converter.Backends.HtmlConverter
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Az aktív konverterek listája. Ide kell felvenni az újat, és automatikusan megjelenik 
-- a weboldalon, az 'exporter' menüben, mint lehetséges formátum.
converters :: [Converter]
converters = [MkConverter TxtConverter, MkConverter HtmlConverter]


--------------------------------------------------------------------------------
-- | Exportálási kérés futtatása. Amennyiben nem jár sikerrel,
-- kivételt dob.
runExport :: ExportRequest     -- ^ export kérés
          -> String            -- ^ exportálandó tartalom
          -> IO ExportResponse -- ^ létrejött fájl
runExport (ExportRequest index) content = do
    case listToMaybe (drop index converters) of
      Nothing        -> error "" --throw
      Just converter -> ExportResponse `fmap` exportToRandomFile content converter "download"


--------------------------------------------------------------------------------
-- | Exportálási kérés, a hálózaton keresztül érkezik.        
newtype ExportRequest = ExportRequest
    { exportRequest_index :: Int }
    

--------------------------------------------------------------------------------
-- | Válasz az exportálási kérésre. Az url a létrejött fájlra mutat.
newtype ExportResponse = ExportResponse
    { exportResponse_url :: String }
    

--------------------------------------------------------------------------------
-- | Tartalom exportálása véletlen nevű fájlba.
exportToRandomFile :: String      -- ^ tartalom
                   -> Converter    -- ^ exportáló
                   -> FilePath    -- ^ ebbe a könyvtárba kerül a kész fájl
                   -> IO FilePath -- ^ létrejött fájl elérési útvonala
exportToRandomFile content (MkConverter converter) dir =
    writeToRandomFile dir $ render converter content


--------------------------------------------------------------------------------
-- | Az export menü dinamikus felépítését generálja a weboldalhoz.
{-
exportersSplice :: MonadIO m => m [H.Node]
exportersSplice = return $ [H.Element "ul" [("id", "exportmenu")]
      (map menuItem (zip [0..] converters))]
  where
    menuItem :: (Int, Converter) -> H.Node
    menuItem (idx, MkConverter converter) =
        H.Element "li" [("data-index", T.pack $ show idx )]
          [ H.TextNode $ T.pack $ displayName converter ]
-}

exportersSplice :: (HasHeist b) => SnapletISplice b
exportersSplice = bulletListSplice "exportmenu" converters
  (\(_, MkConverter conv) -> displayName conv)

--------------------------------------------------------------------------------
-- | JSON reprezentáció generálása.
$(deriveJSON (drop 14) ''ExportRequest)
$(deriveJSON (drop 15) ''ExportResponse)

