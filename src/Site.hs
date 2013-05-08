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
import           DocSnap.Repository

import Data.UUID as UUID
import Snap.Snaplet.Session

import  Debug.Trace

import DocSnap.Serialize (serialize, deserialize)
import DocSnap.Internal.Types
import DocSnap.Internal.Utilities
import Data.Monoid (mempty)
import Control.Concurrent.MVar
import DocSnap.Snap.Splices (javascriptsSplice, renderErrorDialog, iconSplice)
import DocSnap.Export
import DocSnap.Formatting (formattingSplice, cssSplice)
import DocSnap.Snap.Utilities (getServerURL, isAjaxRequest)

import qualified DocSnap.Document as D
import qualified DocSnap.AccessProvider as D
import qualified DocSnap.VersionControl as D

import DocSnap.Snaplets

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
{-
handleOpen :: SharedKey     -- ^ az URL-ből kapott megosztókulcs
           -> AppHandler ()
handleOpen sharedKey = do
    rep <- getRepository
    acc <- access rep sharedKey
    case acc of
      Denied -> notFoundDialog sharedKey
      Granted (DocumentAccess (right,_)) -> renderWithSplices "main"
        [ ("heistscripts", javascriptsSplice "/static/js/" (scripts right) )
        , ("image", iconSplice (icon right) 20)]
  where
    notFoundDialog sk = getServerURL sk >>= \url -> 
      renderErrorDialog ("The following document doesn't exist:\\n" ++ T.unpack url) "ok"
    scripts Author = ["author", "sync"] --szerző esetén az author.js-t is betöltjük
    scripts Reader = ["sync"]
    icon Author = "/static/images/author-icon.png"
    icon Reader = "/static/images/reader-icon.png"
    -}
    
    
handleOpen :: D.SharedKey     -- ^ az URL-ből kapott megosztókulcs
           -> AppHandler ()
handleOpen key = trace "open" $ do
    acc <- with accessProvider $ D.tryAccess key
    case acc of
      Nothing -> notFoundDialogSK key
      Just (D.Access level _) -> renderWithSplices "main"
        [ ("heistscripts", javascriptsSplice "/static/js/" (scripts level) )
        , ("image", iconSplice (icon level) 20)]
  where
    scripts D.Author = ["author", "sync"] --szerző esetén az author.js-t is betöltjük
    scripts D.Reader = ["sync"]
    icon D.Author = "/static/images/author-icon.png"
    icon D.Reader = "/static/images/reader-icon.png"

notFoundDialog sk = getServerURL sk >>= \url -> 
  renderErrorDialog ("The following document doesn't exist:\\n" ++ T.unpack url) "ok"


notFoundDialogSK = notFoundDialog . T.pack . UUID.toString
--------------------------------------------------------------------------------
-- | Új dokumentum - kezelő. Akkor fut le, amikor a felhasználó új dokumentum
-- létrehozását kezdeményezte. Ezt vagy a főoldalon keresztül tudja megtenni,
-- vagy pedig az Eszköztáron a 'New' gombra kattintva, ami után megerősítést
-- kérünk.
handleNew :: AppHandler ()
handleNew = do
    doc <- with versionControl $ D.createRepository
    sk <- with accessProvider $ D.makeAccessible D.Author doc
    redirect $ encodeUtf8 $ D.encodeSharedKey sk

            
--------------------------------------------------------------------------------
-- | Ez a kezelő fut le akkor, amikor a kliens először csatlakozik egy
-- dokumentumhoz, és le akarja tölteni a teljes szöveget.
handleAjaxInitialCheckout :: D.DocumentAccess  -- ^ dokumentumhozzáférés
                          -> AppHandler ()
handleAjaxInitialCheckout (D.Access _ doc) = trace "init" $ do
    curRev <- D.checkout doc
    writeJSON $ D.UpdateResponse curRev [] (-1)

{-
handleAjaxInitialCheckout :: DocumentAccess  -- ^ dokumentumhozzáférés
                          -> AppHandler ()
handleAjaxInitialCheckout (DocumentAccess(_,mdoc)) = do
    revs <- getRevisions mdoc
    let curRev = maybe (Revision 0 []) id $ seqMergeRevisions revs
    writeBS $ serialize $ UpdateResponse curRev [] (-1)
-}
--------------------------------------------------------------------------------
-- | Változások feltöltése, letöltése - kezelő. Ez a legfontosabb kezelő függvény,
-- Végtelenítve hívódik, ez vezérli a dokumentumban történt változtatások
-- beolvasztását.
-- Ha a jogosultságnak nem megfelelő műveletet hajt végre a felhasználó, annak az
-- eredménye egy hibaüzenet a kliens oldalon.
{-
handleAjaxContentUpdate :: (MonadSnap m)
                        => DocumentAccess -- ^ dokumentumhozzáférés
                        -> Arguments      -- ^ frissítési argumentumok
                        -> m ()
handleAjaxContentUpdate (DocumentAccess (right, mdoc)) requestData = do
    case deserialize requestData of
      Nothing -> respondUnknownAjaxError
      Just (Request rev chatName chatBuffer chatVersion) -> do
        doc <- liftIO $ takeMVar mdoc
        let doc' = foldl sendChatMessage doc $ map (ChatMessage chatName) chatBuffer
            (newChatVersion,newChatMessages) = receiveChatMessages doc' chatVersion         
        liftIO $ putMVar mdoc doc'
        rev <- update mdoc rev
        let response = UpdateResponse rev newChatMessages newChatVersion
        writeBS $ serialize response
-}

handleAjaxContentUpdate :: (MonadSnap m)
                        => D.DocumentAccess     -- ^ hozzáférés
                        -> Arguments      -- ^ frissítési argumentumok
                        -> m ()
handleAjaxContentUpdate (D.Access level doc) requestData = do
    case deserialize requestData of
      Nothing -> respondUnknownAjaxError
      Just (D.Request rev chatName chatBuffer chatVersion) -> do
--          doc <- liftIO $ takeMVar mdoc
--          let doc' = foldl sendChatMessage doc $ map (ChatMessage chatName) chatBuffer
--              (newChatVersion,newChatMessages) = receiveChatMessages doc' chatVersion         
--          liftIO $ putMVar mdoc doc'
          resultRev <- D.update doc rev
          writeJSON $ D.UpdateResponse resultRev [] chatVersion --newChatMessages newChatVersion  
        
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
    writeJSON $ ShareResponse $ T.unpack url

handleAjaxAuthorShare :: D.DocumentAccess -> AppHandler ()
handleAjaxAuthorShare (D.Access level doc) = do
    case level of
        D.Reader -> writeBS $ serialize $ ErrorAjaxResponse "You don't have the required permissions to complete this task."
        D.Author -> do
            sk <- with accessProvider $ D.makeAccessible D.Author doc
            url <- getServerURL $ D.encodeSharedKey sk
            writeJSON $ ShareResponse $ T.unpack url


--------------------------------------------------------------------------------
-- | Exportálás txt\/html\/stb. formátumokba - kezelő. A weboldalon az 'export'
-- gombhoz tartozik ez a kezelő. Feladata a választott formátumnak megfelelő
-- fájl előállítása, és az URL visszaküldése a felhasználónak.
handleAjaxExport :: D.DocumentAccess    -- ^ dokumentumhozzáférés
                 -> Arguments         -- ^ export kimenet leírása
                 -> AppHandler ()
handleAjaxExport (D.Access _ doc) json = do
    case deserialize json of
      Nothing -> respondUnknownAjaxError
      Just exportRequest -> do
          content <- D.rawContent doc
          exportResponse <- liftIO $ runExport exportRequest content
          writeJSON $ exportResponse


--------------------------------------------------------------------------------
-- | Az ajax kérések elágaztatásáért felelős kezelő. Akkor hívódik meg, ha
-- egy ajax kérés érkezik egy dokumentum megosztási címére.
{-
type Arguments = B.ByteString
handleAjaxCommand :: D.SharedKey        -- ^ megosztási kulcs
                  -> Maybe Arguments  -- ^ esetleges utasítás-argumentumok
                  -> B.ByteString     -- ^ utasítás
                  -> AppHandler ()
handleAjaxCommand sharedKey maybeArgs command = do
    repo <- getRepository
    acc <- access repo sharedKey
    case acc of
      Denied      -> respondUnknownAjaxError
      Granted granted -> case command of
          "init"   -> handleAjaxInitialCheckout granted
          "update" -> handleAjaxContentUpdate granted (fromJust maybeArgs) --WARNING
          "share"  -> handleAjaxShare granted (fromJust maybeArgs) --WARNING
          "export" -> handleAjaxExport granted (fromJust maybeArgs) --WARNING
          _        -> respondUnknownAjaxError
-}
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
                  "init"   -> handleAjaxInitialCheckout accessRes
                  "update" -> maybe respondUnknownAjaxError (handleAjaxContentUpdate accessRes) maybeArgs 
                  "share"  -> maybe respondUnknownAjaxError (handleAjaxShare accessRes) maybeArgs
                  "export" -> maybe respondUnknownAjaxError (handleAjaxExport accessRes) maybeArgs
                  _        -> respondUnknownAjaxError

--------------------------------------------------------------------------------
-- | Kezelőfüggvényeket rendel a webes útvonalakhoz 
-- Visszatérési értéke egy (Útvonal, Kezelő) alakú párokból álló lista
routes :: [(B.ByteString, AppHandler ())]
routes = [ ("/", ifTop handleIndex)
         , ("/new", handleNew)
         , ("/:sk/",handleDocument)
         , ("/download/", serveDirectory "download")
         , ("/static/", serveDirectory "static" )
         ]    
    
handleDocument = do 
    skbs <- getParam "sk" >>= return . fromJust
    maybeArgs <- getParam "args"
    case D.decodeSharedKey skbs of
        Nothing -> notFoundDialog $ decodeUtf8 skbs
        Just sk -> do
            maybeCmd <- getParam "cmd"
            case maybeCmd of
                Nothing  -> handleOpen sk
                Just cmd -> handleAjaxCommand sk maybeArgs cmd


--------------------------------------------------------------------------------
-- | Inicializálja az alkalmazást
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "heist" heist $ heistInit' "templates" $
      mempty { hcLoadTimeSplices = defaultLoadTimeSplices }
    v <- nestSnaplet "versioncontrol" versionControl $ vcInit
    a <- nestSnaplet "accessprovider" accessProvider $ accProvInit
    addRoutes routes
    addSplices
      [ ("exporters", exportersSplice)
      , ("formatting", formattingSplice)
      , ("heiststyles", cssSplice)]
    return $ App h v a --d


--------------------------------------------------------------------------------
-- | Ismeretlen hiba jelzése AJAX kérés esetén
respondUnknownAjaxError :: (MonadSnap m) => m ()
respondUnknownAjaxError = writeBS $ serialize $ ErrorAjaxResponse "The service encountered an unknown error."

--------------------------------------------------------------------------------
-- | Egyszerű konverziós függvény
bsToStr :: B.ByteString -> String
bsToStr =  T.unpack . decodeUtf8

writeJSON :: (A.ToJSON a, MonadSnap m) => a -> m ()
writeJSON = writeBS . serialize


