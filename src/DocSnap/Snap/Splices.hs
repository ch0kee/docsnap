--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- | Splice konstruktorok a Snap keretrendszerhez.
-- Ezek azok a kis "szeletek", amiket be lehet szúrni a Html
-- kódba. 
module DocSnap.Snap.Splices
  ( HasHeist
  , SnapletISplice
  , bulletListSplice
  , javascriptsSplice
  , renderErrorDialog
  , renderDialog
  , iconSplice
  ) where
  
--------------------------------------------------------------------------------
import qualified Text.XmlHtml as H (Node(..))
import Snap.Snaplet.Heist
import  qualified Data.Text as T
import Snap.Snaplet
import Control.Monad (liftM2)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | Rendezetlen lista Splice (\<ul>), a listalemeknek a data-index attribútumát
-- 0.tól kezdve a megfelelő indexre állítja be.
-- Segítségével + JQuery UI-val tudunk dinamikusan felépülő menüt kódolni.
bulletListSplice :: (HasHeist b) 
                 => T.Text                -- ^ a lista (@<ul>@ html tag) id-je
                 -> [a]                   -- ^ a lista elemei
                 -> ((Int, a) -> String)  -- ^ a listán belüli indexhez és az elemhez a szöveg
                 -> SnapletISplice b
bulletListSplice ulid items text =
    let
      listItem v@(idx, _) = H.Element "li" [("data-index", T.pack $ show idx )]
        [ H.TextNode $ T.pack $ text v ]
    in
      return $ [H.Element "ul" [("id", ulid)]  (map listItem (zip [0..] items))]
      
      
--------------------------------------------------------------------------------
-- | JavaScript \<script> hivatkozások beszúrása.
javascriptsSplice :: (HasHeist b)
                  => String       -- ^ a szkripteket fájlokat tartalmazó mappa
                  -> [FilePath]   -- ^ szkriptek .js kiterjesztés nélkül 
                  -> SnapletISplice b
javascriptsSplice prefix scripts = return $ map (includeJS prefix) scripts 
  where
    includeJS :: String -> FilePath -> H.Node
    includeJS prefix script = H.Element "script" [("src", T.pack $ prefix ++ script ++ ".js")] []


--------------------------------------------------------------------------------
-- | Olyan modális dialog Splice, amit ha a felhasználó elfogadott,
-- visszairányítjuk a kezdőlapra
renderErrorDialog :: (HasHeist b) => String -> String -> Handler b v ()
renderErrorDialog content button = renderDialog content button "/"


--------------------------------------------------------------------------------
-- | Tetszőleges modális párbeszédablak, amely egy megadott url-re irányítja
-- át a felhasználót az elfogadása után.
renderDialog :: (HasHeist b) => String -> String -> String -> Handler b v ()
renderDialog content button target = renderWithSplices "main"
    [ ("heistscripts", liftM2 (++) (vardeclSplice content button target) dialogSplice)]
  where
    dialogSplice :: HasHeist b => SnapletISplice b
    dialogSplice = javascriptsSplice "/static/js/" ["staticdialog"]
    
    vardeclSplice :: HasHeist b => String -> String -> String -> SnapletISplice b
    vardeclSplice content button target = return $
      [ H.Element "script" []
        [ H.TextNode $ T.pack $ concat
          [ "var __dlgContent=\""
          , content
          ,"\",__dlgButton=\""
          , button
          , "\",__dlgTarget=\""
          , target
          , "\";" ] ] ]

--------------------------------------------------------------------------------
-- | Ikont, képet megjelenítő splice
iconSplice :: (HasHeist b)
           => String      -- ^ kép elérési útvonala
           -> Int         -- ^ kép mérete
           -> SnapletISplice b
iconSplice path s = return $ [H.Element "img" 
      [ ("src", T.pack $ path)
      , ("width", T.pack $ show s)
      , ("height", T.pack $ show s) ] []]
      
