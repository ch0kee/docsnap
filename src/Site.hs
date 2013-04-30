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
import System.IO
import  Control.Monad.Trans
import           Control.Applicative hiding (empty)
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
import qualified Heist.Interpreted as I
import          Control.Monad.State
--------------------------------------------------------------------------------
import           Application
import           DocSnap.Repository

import qualified Text.XmlHtml as X

import Snap.Snaplet.Session

import DocSnap.Serialize
import DocSnap.Internal.Types
import DocSnap.Internal.Utilities
import Snap.Extras.SpliceUtils
import Debug.Trace
import Data.Monoid (mempty)
import Control.Concurrent.MVar
import System.FilePath --scriptSplice
import DocSnap.Export
import DocSnap.Export.Converter
import DocSnap.Export.Converter.Backends.TxtConverter
import DocSnap.Export.Converter.Backends.HtmlConverter
import System.FilePath
import System.Directory

import qualified Text.XmlHtml as H  --scriptSplice


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
handleOpen :: SharedKey -> AppHandler ()
handleOpen sharedKey = trace "HANDLE_OPEN" $ do
    rep <- getRepository
    acc <- access rep sharedKey
    case acc of
      Denied -> notFoundDialog $ T.unpack sharedKey
      Granted (DocumentAccess (right,_)) -> renderWithSplices "main"
        [ ("heistscripts", javascriptsSplice "/static/js/" (scripts right) ) ]
  where
    notFoundDialog sk = renderErrorDialog
      ("The following document doesn't exist:\\n" ++ urlOnSite sk) "ok"
    scripts Author = ["author", "sync"]
    scripts Reader = ["sync"]


--------------------------------------------------------------------------------
-- | Új dokumentum - kezelő. Akkor fut le, amikor a felhasználó új dokumentum
-- létrehozását kezdeményezte. Ezt vagy a főoldalon keresztül tudja megtenni,
-- vagy pedig az Eszköztáron a 'New' gombra kattintva, ami után megerősítést
-- kérünk.
handleNew :: AppHandler ()
handleNew = trace "HANDLE_NEW" $ do
    repo <- getRepository
    newDoc <- createDocument repo
    sk <- shareDocument repo $ DocumentAccess (Author, newDoc)
    redirect $ encodeUtf8 sk


--------------------------------------------------------------------------------
-- | Az ajax kérések elágaztatásáért felelős kezelő. Akkor hívódik meg, ha
-- egy ajax kérés érkezik egy dokumentum megosztási címére.
type Arguments = B.ByteString
handleAjaxCommand :: SharedKey -> Maybe Arguments -> B.ByteString -> AppHandler ()
handleAjaxCommand sharedKey maybeArgs command = trace ("HANDLE_COMMAND "++ bsToStr command) $ do
    repo <- getRepository
    acc <- access repo sharedKey
    case acc of
      Denied      -> return () --showFatalErrorDialog() (browser error)
      Granted granted -> case command of
          "init"   -> handleAjaxInitialCheckout granted
          "update" -> handleAjaxContentUpdate granted (fromJust maybeArgs) --WARNING
          "share"  -> handleAjaxShare granted (fromJust maybeArgs) --WARNING
          "export" -> handleAjaxExport granted (fromJust maybeArgs) --WARNING
          _        -> return () --showFatalErrorDialog() (browser error)


--------------------------------------------------------------------------------
-- | Ez a kezelő fut le akkor, amikor a kliens először csatlakozik egy
-- dokumentumhoz, és le akarja tölteni a teljes szöveget.
handleAjaxInitialCheckout :: DocumentAccess -> AppHandler ()
handleAjaxInitialCheckout (DocumentAccess(_,mdoc)) = trace "HANDLE_INITIAL_CHECKOUT_" $ do
    revs <- getRevisions mdoc
    let curRev = maybe (Revision 0 []) id $ seqMergeRevisions revs
    writeBS $ serialize $ UpdateResponse curRev [] (-1)


--------------------------------------------------------------------------------
-- | Változások feltöltése, letöltése - kezelő. Ez a legfontosabb kezelő függvény,
-- Végtelenítve hívódik, ez vezérli a dokumentumban történt változtatások
-- beolvasztását.
-- Ha a jogosultságnak nem megfelelő műveletet hajt végre a felhasználó, akkor
-- kivételt váltunk ki, aminek az eredménye egy információs párbeszédablak
-- a kliens oldalon.
handleAjaxContentUpdate :: (MonadSnap m) => DocumentAccess -> Arguments -> m ()
handleAjaxContentUpdate (DocumentAccess (right, mdoc)) requestData = trace "HANDLE_CONTENT_UPDATE" $ do
    case deserialize requestData of
      Nothing -> trace "bad request data" $ error "bad request data"
      Just (Request rev chatName chatBuffer chatVersion)  -> do
        doc <- liftIO $ takeMVar mdoc
        let doc' = foldl sendChatMessage doc $ map (ChatMessage chatName) chatBuffer
            (newChatVersion,newChatMessages) = receiveChatMessages doc' chatVersion         
        liftIO $ putMVar mdoc doc'
        rev <- commit mdoc rev
        let response = UpdateResponse rev newChatMessages newChatVersion
        writeBS $ serialize response
        
        
--------------------------------------------------------------------------------
-- | Megosztás - kezelő. Ez a kezelő függvény felelős a megosztások
-- beregisztrálásáért, attól függően, hogy Olvasó, vagy Szerkesztő
-- jogosultsággal rendelkezik a felhasználó.
handleAjaxShare :: DocumentAccess -> Arguments -> AppHandler ()
handleAjaxShare acc shareType = trace "HANDLE_SHARE" $ do
    case deserialize shareType of
      Just (ShareRequest "reader") -> handleAjaxReaderShare acc
      Just (ShareRequest "author") -> handleAjaxAuthorShare acc
      _ -> trace "INVALID REQUEST" return ()

handleAjaxReaderShare :: DocumentAccess -> AppHandler ()
handleAjaxReaderShare (DocumentAccess (right,mdoc)) = trace "HANDLE_READER_SHARE" $ do
    url <- maybe `liftM` return "<server-url>" `ap` return bsToStr `ap` withRequest (return . getHeader "Origin")
    dh <- getRepository
    sk <- shareDocument dh $ DocumentAccess (Reader, mdoc)
    writeBS $ serialize $ ShareResponse (url ++ T.unpack sk)

handleAjaxAuthorShare :: DocumentAccess -> AppHandler ()
handleAjaxAuthorShare (DocumentAccess (right,mdoc)) = trace "HANDLE_READER_SHARE" $ do
    url <- maybe `liftM` return "<server-url>" `ap` return bsToStr `ap` withRequest (return . getHeader "Origin")
    case right of
        Reader -> return () --show generalfaultdialog()
        Author -> do
            dh <- getRepository
            sk <- shareDocument dh $ DocumentAccess (Author, mdoc)
            writeBS $ serialize $ ShareResponse (url ++ T.unpack sk)


--------------------------------------------------------------------------------
-- | Exportálás txt\/html\/stb. formátumokba - kezelő. A weboldalon az 'export'
-- gombhoz tartozik ez a kezelő. Feladata a választott formátumnak megfelelő
-- fájl előállítása, és egy letöltési kérelem küldése a felhasználónak.
-- Az export alrendszer működéséről bővebben lásd az Export modult.
handleAjaxExport :: DocumentAccess -> Arguments -> AppHandler ()
handleAjaxExport (DocumentAccess(_,mdoc)) json = trace "HANDLE_EXPORT" $ do
    case deserialize json of
      Nothing -> suppressError
      Just exportRequest -> do
          revs <- getRevisions mdoc
          let content = getContent revs          
          exportResponse <- liftIO $ runExport exportRequest content
          writeBS . serialize $ exportResponse
  where
    getContent :: [Revision] -> String
    getContent revs = maybe "" (concat . map extract . edits) $ seqMergeRevisions revs
      where
        extract (I str) = str
        extract _       = ""
    suppressError :: AppHandler ()
    suppressError = return ()



--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
sharePrefix = "http://localhost:8000/"

urlOnSite s = sharePrefix ++ s

--data Insert = Insert { index :: Int, content :: String }
--  deriving (Show)

bsToStr :: B.ByteString -> String
bsToStr =  T.unpack . decodeUtf8



 



--traceParams = getParams >>= \params -> logDSS $ "params=" ++ show params

  
-- | The application's routes.
routes :: [(B.ByteString, AppHandler ())]
routes = [ ("/", ifTop handleIndex)
         , ("/new", handleNew)
         , ("/:sk/",ifTop (getParam "cmd"
                    >>= maybe
                          (sharedKey >>= handleOpen)
                          (\cmd -> join (handleAjaxCommand `liftM` sharedKey `ap` args `ap` return cmd))))
         , ("/download/", serveDirectory "download")
         , ("/static/", serveDirectory "static" )
         ]
  where
    sharedKey :: AppHandler SharedKey
    sharedKey = getParam "sk" >>= return . decodeUtf8 . fromJust
    args :: AppHandler (Maybe B.ByteString)
    args = getParam "args"
    
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
--    wrapSite (\site -> logHandlerStart >> traceParams >> site >> logHandlerFinished)
    h <- nestSnaplet "heist" heist $ heistInit' "templates" (mempty { hcLoadTimeSplices = defaultLoadTimeSplices })
    s <- nestSnaplet "sess" session $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    r <- nestSnaplet "repository" repository $ repositoryInit
    addRoutes routes
    addSplices [("exporters", exportersSplice)]
    return $ App h s r




--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--SNAP modul
javascriptsSplice :: HasHeist b => String -> [FilePath] -> SnapletISplice b
javascriptsSplice prefix scripts = return $ map (includeJS prefix) scripts 
  where
    includeJS :: String -> FilePath -> H.Node
    includeJS prefix script = H.Element "script" [("src", T.pack $ prefix ++ script ++ ".js")] []

renderErrorDialog :: HasHeist b => String -> String -> Handler b v ()
renderErrorDialog content button = renderDialog content button "/"

renderDialog :: HasHeist b => String -> String -> String -> Handler b v ()
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



