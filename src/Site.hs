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
-- | Főoldal, kezdőlap - kezelő
handleIndex :: AppHandler ()
handleIndex = renderWithSplices "main" [("heistscripts", openNewDialogSplice)]
  where
    openNewDialogSplice = javascriptsSplice "/static/js/" ["createnewdlg"]
  
  
--------------------------------------------------------------------------------
-- | Dokumentum megnyitása - kezelő
handleOpen :: SharedKey -> AppHandler ()
handleOpen sharedKey = trace "HANDLE_OPEN" $ do
    dh <- getRepository
    acc <- access dh sharedKey
    case acc of
      Denied -> notFoundDialog $ T.unpack sharedKey
      Granted (DocumentAccess (right,_)) -> renderWithSplices "main"
        [ ("heistscripts", javascriptsSplice "/static/js/" (scripts right) ) ]
  where
    notFoundDialog sk = renderErrorDialog
      ("The following document doesn't exist:\\n" ++ urlOnSite sk) "Ok"
    scripts Author = ["author", "sync"]
    scripts Reader = ["sync"]


--------------------------------------------------------------------------------
-- | Változások feltöltése, letöltése - kezelő
handleContentUpdate :: (MonadSnap m) => DocumentAccess -> Arguments -> m ()
handleContentUpdate (DocumentAccess (right, mdoc)) requestData = trace "HANDLE_CONTENT_UPDATE" $ do
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
        logDSS ("sent " ++ bsToStr (serialize response)) 


--------------------------------------------------------------------------------
-- | Új dokumentum - kezelő
handleNew :: AppHandler ()
handleNew = trace "HANDLE_NEW" $ do
    dh <- getRepository
    newDoc <- createDocument dh
    sk <- shareDocument dh $ DocumentAccess (Author, newDoc)
    redirect $ getAccessURI sk


--------------------------------------------------------------------------------
-- | Megosztás - kezelő
handleShare :: DocumentAccess -> Arguments -> AppHandler ()
handleShare acc shareType = trace "HANDLE_SHARE" $ do
    case deserialize shareType of
      Just (ShareRequest "reader") -> handleReaderShare acc
      Just (ShareRequest "author") -> handleAuthorShare acc
      _ -> trace "INVALID REQUEST" return ()

handleReaderShare :: DocumentAccess -> AppHandler ()
handleReaderShare (DocumentAccess (right,mdoc)) = trace "HANDLE_READER_SHARE" $ do
    url <- maybe `liftM` return "<server-url>" `ap` return bsToStr `ap` withRequest (return . getHeader "Origin")
    dh <- getRepository
    sk <- shareDocument dh $ DocumentAccess (Reader, mdoc)
    writeBS $ serialize $ ShareResponse (url ++ T.unpack sk)
    --return ()

handleAuthorShare :: DocumentAccess -> AppHandler ()
handleAuthorShare (DocumentAccess (right,mdoc)) = trace "HANDLE_READER_SHARE" $ do
    url <- maybe `liftM` return "<server-url>" `ap` return bsToStr `ap` withRequest (return . getHeader "Origin")
    case right of
        Reader -> return () --show generalfaultdialog()
        Author -> do
            dh <- getRepository
            sk <- shareDocument dh $ DocumentAccess (Author, mdoc)
            logDSS $ "sending response"
            writeBS $ serialize $ ShareResponse (url ++ T.unpack sk)

    
-- | Azokban az esetekben hívjuk, amikor bár hiba történt,
-- nem reagálunk rá semmit

suppressError :: AppHandler ()
suppressError = return ()

handleExport :: DocumentAccess -> Arguments -> AppHandler ()
handleExport (DocumentAccess(_,mdoc)) json = trace "HANDLE_EXPORT" $ do
    case deserialize json of
      Nothing -> suppressError
      Just exportRequest -> do
          revs <- getRevisions mdoc
          let content = getContent revs          
          exportResponse <- liftIO $ runExport exportRequest content
          writeBS . serialize $ exportResponse

handleInitialCheckout :: DocumentAccess -> AppHandler ()
handleInitialCheckout (DocumentAccess(_,mdoc)) = trace "HANDLE_INITIAL_CHECKOUT_" $ do
--    dh <- getRepository
--    doc <- accessAs dh Author
    revs <- getRevisions mdoc
    logDSS $ show revs
    let curRev = maybe (Revision 0 []) id $ seqMergeRevisions revs
    logDSS $ show curRev  
    writeBS $ serialize $ UpdateResponse curRev [] (-1)
    logDSS "initial checkout handled"

type Arguments = B.ByteString
handleCommand :: SharedKey -> Maybe Arguments -> B.ByteString -> AppHandler ()
handleCommand sharedKey maybeArgs command = trace ("HANDLE_COMMAND "++ bsToStr command) $ do
    dh <- getRepository
    acc <- access dh sharedKey
    case acc of
      Denied      -> return () --showFatalErrorDialog() (browser error)
      Granted granted -> case command of
          "init"   -> handleInitialCheckout granted
          "update" -> handleContentUpdate granted (fromJust maybeArgs) --WARNING
          "share"  -> handleShare granted (fromJust maybeArgs) --WARNING
          "export" -> handleExport granted (fromJust maybeArgs) --WARNING
          _        -> return () --showFatalErrorDialog() (browser error)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
sharePrefix = "http://localhost:8000/"

urlOnSite s = sharePrefix ++ s

--data Insert = Insert { index :: Int, content :: String }
--  deriving (Show)

bsToStr :: B.ByteString -> String
bsToStr =  T.unpack . decodeUtf8

strToBs :: String -> B.ByteString
strToBs =  encodeUtf8 . T.pack


logDSSb c s = liftIO $ putStrLn (c:"** " ++ s)
logDSSa c s = liftIO $ putStrLn ("** " ++ s ++ [c])

--visszaküld kezdőlapra hiba esetén
maybeDenied ::  (a -> AppHandler b) -> Maybe a -> AppHandler b
maybeDenied = maybe (redirect "/")


--modul: Document


      


renderErrorDialog content button = renderDialog content button "/"

renderDialog content button target = renderWithSplices "main"
    [ ("heistscripts", liftM2 (++) (vardeclSplice content button target) dialogSplice)]
  where
    dialogSplice = javascriptsSplice "/static/js/" ["staticdialog"]
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

javascriptsSplice :: MonadIO m => String -> [FilePath] -> m [H.Node]
javascriptsSplice prefix scripts = return $ map (includeJS prefix) scripts 
  where
    includeJS :: String -> FilePath -> H.Node
    includeJS prefix script = H.Element "script" [("src", T.pack $ prefix ++ script ++ ".js")] []
  
getAccessURI :: SharedKey -> B.ByteString
getAccessURI = encodeUtf8



{-
exporters :: [Converter]
exporters = [Converter TxtConverter, Converter HtmlConverter]  
-}


getContent :: [Revision] -> String
getContent revs = maybe "" (concat . map extract . edits) $ seqMergeRevisions revs
  where
    extract (I str) = str
    extract _       = ""



traceParams = getParams >>= \params -> logDSS $ "params=" ++ show params

  
-- | The application's routes.
routes :: [(B.ByteString, AppHandler ())]
routes = [ ("/", ifTop handleIndex)
         , ("/new", handleNew)
         , ("/:sk/",ifTop (getParam "cmd"
                    >>= maybe
                          (sharedKey >>= handleOpen)
                          (\cmd -> join (handleCommand `liftM` sharedKey `ap` args `ap` return cmd))))
         , ("/download/", serveDirectory "download")
         , ("/static/", rlogDSS "tryServeStatic" >> serveDirectory "static" >> rlogDSS "ok")
         ]
  where
    sharedKey :: AppHandler SharedKey
    sharedKey = getParam "sk" >>= return . decodeUtf8 . fromJust
    args :: AppHandler (Maybe B.ByteString)
    args = getParam "args"
    


logHandlerStart = logDSSb '\n' "HANDLER STARTED"
logHandlerFinished = logDSSa '\n' "HANDLER FINISHED"

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    wrapSite (\site -> logHandlerStart >> traceParams >> site >> logHandlerFinished)
    h <- nestSnaplet "heist" heist $ heistInit' "templates" (mempty { hcLoadTimeSplices = defaultLoadTimeSplices })
    s <- nestSnaplet "sess" session $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    r <- nestSnaplet "repository" repository $ repositoryInit
    addRoutes routes
    addSplices [("exporters", exportersSplice)]
    return $ App h s r


--todo: UTILS  
lbsToBs :: BL.ByteString -> B.ByteString
lbsToBs = B.pack . BL.unpack

bsToLbs :: B.ByteString -> BL.ByteString
bsToLbs = BL.pack . B.unpack

--kulcs lekérése
--todo: put this in where handleOpen
getSharedKey :: MonadSnap m => m (Maybe SharedKey)
getSharedKey = do { msk <- getParam "sk"; return $ msk >>= Just . decodeUtf8 }



