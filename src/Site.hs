{-# OPTIONS_HADDOCK ignore-exports #-}
--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- | Ebben a modulban van az URL útvonalak feloldásáért felelős /route/
-- függvény, továbbá az egyes oldalak kezelésért felelős @handler*@ függvények.
-- Az 'app' függvényben történik az egységek összekapcsolása, ez az egyetlen
-- exportált függvény ebből a modulból.
module Site
  ( app
  ) where

--------------------------------------------------------------------------------
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import qualified Data.Text as T
import    Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Heist
import          Control.Monad.State
import           Control.Monad.Trans.Maybe
import  qualified Data.Aeson as A
--------------------------------------------------------------------------------
import           Application

import Data.UUID as UUID
import Snap.Snaplet.Session


import DocSnap.Serialize (serialize, deserialize)
import Data.Monoid (mempty)
import Control.Concurrent.MVar
import DocSnap.Splices (javascriptsSplice, renderErrorDialog, iconSplice)
import DocSnap.Export
import DocSnap.Formatting (formattingSplice, cssSplice)
import DocSnap.Utilities

import qualified DocSnap.Document as D
import qualified DocSnap.AccessProvider as D
import qualified DocSnap.VersionControl as D
import DocSnap.Synchronize

--import  Snap.Snaplet.MongoDB
--import Database.MongoDB.Connection

--------------------------------------------------------------------------------
-- | Főoldal, kezdőlap - kezelő. Ez a függvény fut le akkor, ha a felhasználó
-- a weboldal gyökerébe navigál.
-- Megjelenít egy modális párbeszédablakot egy "Create New Dialog" gombbal
-- a közepén, a weboldal semmilyen más funkciója nem érhető el eközben.
-- Ha nem létező dokumentumot próbál megnyitni, akkor is
-- ide irányítjuk át, de előtte tájékoztatjuk a megnyitás sikertelenségéról.
handleIndex :: AppHandler ()
handleIndex = renderWithSplices "main" [("heistscripts", openNewDialogSplice)]
  where
    openNewDialogSplice = javascriptsSplice "/static/js/" ["createnewdlg"]
  
  
--------------------------------------------------------------------------------
-- | Dokumentum megnyitása - kezelő. Akkor hívódik meg, amikor a felhasználó
-- (feltehetően) csatlakozni akar egy dokumentumhoz. Ha nem létezik a dokumentum,
-- akkor azt egy párbeszédablakban jelezzük.
-- A megosztáshoz tartozó jogosultságnak megfelelő eszközöket töltünk be az
-- oldalra, így például Olvasó jogosultsággal nem fogja tudni szerkeszteni
-- a dokumentumot. 

    
handleOpen :: D.SharedKey     -- ^ az URL-ből kapott megosztókulcs
           -> AppHandler ()
handleOpen key = do
    acc <- with accessProvider $ D.tryAccess key
    case acc of
      Nothing -> notFoundDialog $ D.encodeSharedKey key
      Just (D.Access level _) -> renderWithSplices "main"
        [ ("heistscripts", javascriptsSplice "/static/js/" (scripts level) )
        , ("image", iconSplice (icon level) 20)]
  where
    scripts D.Author = ["author", "sync"] --szerző esetén az author.js-t is betöltjük
    scripts D.Reader = ["sync"]
    icon D.Author = "/static/images/author-icon.png"
    icon D.Reader = "/static/images/reader-icon.png"

--------------------------------------------------------------------------------
-- | Új dokumentum - kezelő. Akkor fut le, amikor a felhasználó új dokumentum
-- létrehozását kezdeményezte. Ezt vagy a főoldalon keresztül tudja megtenni,
-- vagy pedig az Eszköztáron a 'New' gombra kattintva, ami után megerősítést
-- kérünk.
handleNew :: AppHandler ()
handleNew = do
    doc <- with versionControl $ D.createDocument
    sk <- with accessProvider $ D.makeAccessible D.Author doc
    redirect $ encodeUtf8 $ D.encodeSharedKey sk


--------------------------------------------------------------------------------
-- | Változások feltöltése, letöltése - kezelő. Ez a legfontosabb kezelő függvény,
-- Végtelenítve hívódik, ez vezérli a dokumentumban történt változtatások
-- beolvasztását.
-- Ha a jogosultságnak nem megfelelő műveletet hajt végre a felhasználó, annak az
-- eredménye egy hibaüzenet a kliens oldalon.


handleAjaxUpdate :: D.DocumentAccess     -- ^ hozzáférés
                        -> Arguments      -- ^ frissítési argumentumok
                        -> AppHandler ()
handleAjaxUpdate (D.Access level doc) requestData = with ajaxSync $ do
    receive Editor >>= D.update doc >>= send Editor
    receive Chat >>= D.chat doc >>= send Chat
    storePayloads

--------------------------------------------------------------------------------
-- | Megosztás - kezelő. Ez a kezelő függvény felelős a megosztások
-- beregisztrálásáért, attól függően, hogy Olvasó, vagy Szerkesztő
-- jogosultsággal rendelkezik a felhasználó.
handleAjaxShare :: D.DocumentAccess  -- ^ dokumentumhozzáférés
                -> Arguments       -- ^ megosztást leíró argumentum
                -> AppHandler ()
handleAjaxShare acc shareType = do
    case deserialize shareType of
      Just (D.ShareRequest "reader") -> handleAjaxReaderShare acc
      Just (D.ShareRequest "author") -> handleAjaxAuthorShare acc
      _ -> respondUnknownAjaxError

handleAjaxReaderShare :: D.DocumentAccess -> AppHandler ()
handleAjaxReaderShare (D.Access level doc) = do
    sk <- with accessProvider $ D.makeAccessible D.Reader doc
    url <- getServerURL $ D.encodeSharedKey sk
    writeJSON $ D.ShareResponse $ T.unpack url

handleAjaxAuthorShare :: D.DocumentAccess -> AppHandler ()
handleAjaxAuthorShare (D.Access level doc) = do
    case level of
        D.Reader -> writeJSON $ ErrorAjaxResponse "You don't have the required permissions to complete this task."
        D.Author -> do
            sk <- with accessProvider $ D.makeAccessible D.Author doc
            url <- getServerURL $ D.encodeSharedKey sk
            writeJSON $ D.ShareResponse $ T.unpack url


--------------------------------------------------------------------------------
-- | Exportálás txt\/html\/stb. formátumokba - kezelő. A weboldalon az 'export'
-- gombhoz tartozik ez a kezelő. Feladata a választott formátumnak megfelelő
-- fájl előállítása, és az URL visszaküldése a felhasználónak.
handleAjaxExport :: D.DocumentAccess    -- ^ dokumentumhozzáférés
                 -> D.SharedKey       -- ^ exportált fájl neve
                 -> Arguments         -- ^ export kimenet leírása
                 -> AppHandler ()
handleAjaxExport (D.Access _ doc) key json = do
    case deserialize json of
      Nothing -> respondUnknownAjaxError
      Just exportRequest -> do
          content <- D.rawContent doc
          exportResponse <- liftIO $ runExport exportRequest (D.encodeSharedKey key)  content
          writeJSON $ exportResponse


--------------------------------------------------------------------------------
-- | Az ajax kérések elágaztatásáért felelős kezelő. Akkor hívódik meg, ha
-- egy ajax kérés érkezik egy dokumentum megosztási címére.

type Arguments = B.ByteString
type Command = B.ByteString
handleAjaxCommand :: D.SharedKey        -- ^ megosztási kulcs
                  -> Maybe Arguments  -- ^ esetleges utasítás-argumentumok
                  -> Command     -- ^ utasítás
                  -> AppHandler ()
handleAjaxCommand key maybeArgs command = do
    accessRes <- with accessProvider $ D.tryAccess key
    case accessRes of
      Nothing        -> respondUnknownAjaxError
      Just accessRes -> case command of
--                  "init"   -> handleAjaxInitialCheckout accessRes
                  "update" -> maybe respondUnknownAjaxError (handleAjaxUpdate accessRes) maybeArgs 
                  "share"  -> maybe respondUnknownAjaxError (handleAjaxShare accessRes) maybeArgs
                  "export" -> maybe respondUnknownAjaxError (handleAjaxExport accessRes key) maybeArgs
                  _        -> respondUnknownAjaxError

--------------------------------------------------------------------------------
-- | Kezelőfüggvényeket rendel a webes útvonalakhoz 
routes :: [(B.ByteString, AppHandler ())]
routes = [ ("/", ifTop handleIndex)
         , ("/new", handleNew)
         , ("/:sk/",handleDocumentLink)
         , ("/download/", serveDirectory "download")
         , ("/static/", serveDirectory "static" )
--         , ("", redirect "/") --ha semmire sem illeszkedik, irányítsuk a főoldalra
         ]    
    

handleDocumentLink :: AppHandler ()
handleDocumentLink = do 
    skbs <- getParam "sk" >>= return . fromJust
    case D.decodeSharedKey skbs of
      Nothing -> notFoundDialog $ decodeUtf8 skbs
      Just sk -> do
          maybeArgs <- getParam "args"
          getParam "cmd" >>= maybe (handleOpen sk) (handleAjaxCommand sk maybeArgs)


--------------------------------------------------------------------------------
-- | Inicializálja az alkalmazást
app :: SnapletInit App App
app = makeSnaplet "docsnap" "DocSnap MultiUser Text Editing application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit' "templates" $
      mempty { hcLoadTimeSplices = defaultLoadTimeSplices }
    j <- nestSnaplet "ajaxsync" ajaxSync $ initAjaxSynchronize
    v <- nestSnaplet "versioncontrol" versionControl $ D.initVersionControl
    a <- nestSnaplet "accessprovider" accessProvider $ D.initAccessProvider
    addRoutes routes
    addSplices
      [ ("exporters", exportersSplice)
      , ("formatting", formattingSplice)
      , ("heiststyles", cssSplice)]
    return $ App h j v a



--------------------------------------------------------------------------------
-- | segédfüggvények
writeJSON :: (A.ToJSON a, MonadSnap m) => a -> m ()
writeJSON = writeBS . serialize

-- | nem található a dokumentum
notFoundDialog :: T.Text -> AppHandler ()
notFoundDialog sk = getServerURL sk >>= \url -> 
  renderErrorDialog ("The following document doesn't exist:\\n" ++ T.unpack url) "ok"


