--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- | Minden, az IExporter osztályt megvalósító típusEbben a modulban találhatóak
-- az elkészült tartalmak tetszőleges
-- formátumba történő kiexportálásához végző típusok
module DocSnap.Formatting
  ( formattingSplice
  , cssSplice
  , formatCss
  ) where
  
--------------------------------------------------------------------------------
import qualified Text.XmlHtml as H (Node(..))
import Control.Monad.Trans (MonadIO)
import  qualified Data.Text as T 
import  Data.Aeson.TH
--------------------------------------------------------------------------------
import  DocSnap.Splices (bulletListSplice, HasHeist, SnapletISplice)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | A formátumok listája. Ide kell felvenni az újat, és automatikusan megjelenik 
-- a weboldalon a Formázás eszköztáron. Azt a stílust alkalmazza, amelyet
-- a név elé illesztett "ds_" előtaggal kapunk.
formatting :: [Formatting]
formatting =
  [ Formatting "bold"
  , Formatting "italic" 
  , Formatting "underline"
  ]


data Formatting = Formatting { text :: T.Text }

--------------------------------------------------------------------------------
-- | Felhasznált CSS stílusok 
formatCss :: T.Text
formatCss = T.concat
  [ ".ds_bold { font-weight: bold; } "
  , ".ds_italic { font-style: italic; } "
  , ".ds_underline { text-decoration:underline; } "
  ]


--------------------------------------------------------------------------------
-- | A Formázási eszköztár dinamikus felépítését generálja a weboldalhoz.
formattingSplice :: (HasHeist b) => SnapletISplice b
formattingSplice = return $ map (\f -> H.Element "div"
    [("class", "formatting"),("data-class", "ds_" `T.append` text f)] [ H.TextNode $ text f ]) formatting

--------------------------------------------------------------------------------
-- | CSS stílusok dinamikus beillesztése a weblapba
cssSplice :: (HasHeist b) => SnapletISplice b
cssSplice = return $ [H.Element "style" [("type", "text/css")] [H.TextNode formatCss]]

