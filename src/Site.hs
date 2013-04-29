{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------

import           Control.Lens.TH
import System.IO
import  Control.Monad.Trans
import           Control.Applicative hiding (empty)
import Data.ByteString (ByteString)
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
import           Data.Lens.Lazy
import Control.Comonad.Trans.Store
--import Control.Lens
import          Control.Monad.State
------------------------------------------------------------------------------
import           Application
import           DocSnapServer

import qualified Text.XmlHtml as X
import          Data.IORef

import Control.Arrow hiding (app)

import Data.Aeson.TH
import qualified Data.Aeson as A
import Snap.Snaplet.Session

import Serialize
import Internal.Types
import Snap.Extras.SpliceUtils
import Debug.Trace
import Data.Monoid (mempty)
import Control.Monad.Trans.Maybe --del
import Control.Concurrent.MVar.Strict
import System.FilePath --scriptSplice
import Exporter
import Exporter.TxtExporter
import Control.Monad.Random
import Data.Char (chr)
import System.FilePath
import System.Directory

import qualified Text.XmlHtml as H  --scriptSplice
sharePrefix = "http://localhost:8000/"

urlOnSite s = sharePrefix ++ s

--data Insert = Insert { index :: Int, content :: String }
--  deriving (Show)

bsToStr :: ByteString -> String
bsToStr =  T.unpack . decodeUtf8

strToBs :: String -> ByteString
strToBs =  encodeUtf8 . T.pack


logDSSb c s = liftIO $ putStrLn (c:"** " ++ s)
logDSSa c s = liftIO $ putStrLn ("** " ++ s ++ [c])


--accessAsAuthor: megprobalja elerni a doksit a sütin és
--az url-en keresztül, ha nem megy megszakad az aktuális kezelő

--kulcs lekérése
getSharedKey :: AppHandler (Maybe SharedKey)
getSharedKey = do { msk <- getParam "sk"; return $ msk >>= Just . decodeUtf8 }

--visszaküld kezdőlapra hiba esetén
maybeDenied ::  (a -> AppHandler b) -> Maybe a -> AppHandler b
maybeDenied = maybe (redirect "/")

accessAs :: DocumentHost -> AccessRight -> AppHandler (MDocument)
accessAs dh r = getSharedKey >>= maybeDenied (\sk -> tryAccessAs dh sk r >>= maybeDenied return)

--chat üzenet küldése
sendChatMessage :: Document -> ChatMessage -> Document
sendChatMessage doc msg = doc {chatLog=withMessage msg (chatLog doc) }
  where
    withMessage msg [] = [(0, msg)]
    withMessage msg l@((nr,_):_) = (nr+1, msg):l
    
--chat üzenetek fogadása adott verziótól kezdve
receiveChatMessages :: Document -> Version -> (Version,[ChatMessage])
receiveChatMessages doc v = receiveChatMessages' $ takeWhile ((>v)  . fst) $ chatLog doc
  where
    receiveChatMessages' [] = (v, [])
    receiveChatMessages' l@(x:_) = (fst x, map snd l)

-- Lekezeljuk az uj adatot
handleContentUpdate :: AppHandler ()
handleContentUpdate = trace "HANDLE_CONTENT_UPDATE" $ do
    dh <- getDocumentHost
    mdoc <- accessAs dh Author
    getParam "args" >>= maybeDenied (\d -> trace "handle_commit" $ handleCommit mdoc d)
  where
    handleCommit :: MDocument -> B.ByteString -> AppHandler ()
    handleCommit mdoc cdata = do
      case deserialize cdata of
        Nothing -> trace "bad request data" $ error "bad request data"
        Just (Request rev chatBuffer chatVersion)  -> do
          doc <- liftIO $ takeMVar mdoc
          let authorName = "unnamed"
              doc' = foldl sendChatMessage doc $ map (ChatMessage authorName) chatBuffer
              (newChatVersion,newChatMessages) = receiveChatMessages doc' chatVersion         
          liftIO $ putMVar mdoc doc'
          rev <- commit mdoc rev
          let response = Response rev newChatMessages newChatVersion
          writeBS $ serialize response
          logDSS ("sent " ++ bsToStr (serialize response)) 
      
--hibás url esetén feldobunk egy dialogot a hibaüzenettel
handleOpen :: AppHandler ()
handleOpen = trace "HANDLE_OPEN" $ do
    dh <- getDocumentHost
    maybeSharedKey <- getSharedKey
    case maybeSharedKey of
      Nothing -> notFoundDialog ""
      Just sk -> do
        maybeDoc <- tryAccessAs dh sk Author
        case maybeDoc of
          Nothing -> notFoundDialog $ T.unpack sk
          Just doc -> loadExistingDocument
  where
    notFoundDialog sk = renderErrorDialog
      ("The following document doesn't exist:\\n" ++ urlOnSite sk) "ok"
    storeSession sk = with session $ do {resetSession ; setInSession "sk" sk; commitSession}
    loadExistingDocument = renderWithSplices "main" [ ("heistscripts", scripts ) ]
    scripts = javascriptsSplice "/static/js/" ["sync", "author/docsnap-author"]

renderErrorDialog content button = renderDialog content button "/"

renderDialog content button target = renderWithSplices "main"
    [ ("heistscripts", liftM2 (++) (vardeclSplice content button target) dialogSplice)]
  where
    dialogSplice = javascriptsSplice "/static/js/" ["dialog"]
    vardeclSplice content button target = return $ [ H.Element "script" []
      [ H.TextNode $ T.pack $ concat
        [ "var __dlgContent=\""
        , content
        ,"\",__dlgButton=\""
        , button
        , "\",__dlgTarget=\""
        , target
        , "\";"
        ]
      ] ]

javascriptsSplice :: MonadIO m => String -> [FilePath] -> m [H.Node]
javascriptsSplice prefix scripts = return $ map (includeJS prefix) scripts 
  where
    includeJS :: String -> FilePath -> H.Node
    includeJS prefix script = H.Element "script" [("src", T.pack $ prefix ++ script ++ ".js")] []
  
getAccessURI :: SharedKey -> ByteString
getAccessURI = encodeUtf8

handleNew :: AppHandler ()
handleNew = trace "HANDLE_NEW" $ do
    dh <- getDocumentHost
    newDoc <- createDocument dh
    sk <- shareDocument dh $ DocumentAccess (Author, newDoc)
    redirect $ getAccessURI sk

handleShare :: AppHandler ()
handleShare = trace "HANDLE_SHARE" $ do
    shareType <- getParam "args"
    case shareType of
      Just "reader" -> handleReaderShare
      Just "author" -> handleReaderShare -- handleAuthorShare
      _ -> return ()

handleReaderShare :: AppHandler ()
handleReaderShare = trace "HANDLE_READER_SHARE" $ do
    dh <- getDocumentHost
    doc <- accessAs dh Author
    sk <- shareDocument dh $ DocumentAccess (Reader, doc)
    writeBS $ strToBs $ sharePrefix ++ T.unpack sk
    --return ()
    
-- | Azokban az esetekben hívjuk, amikor bár hiba történt,
-- nem reagálunk rá semmit

suppressError :: AppHandler ()
suppressError = return ()

maybeRead = fmap fst . listToMaybe . reads


exporters :: [Exporter]
exporters = [Exporter TxtFormat, Exporter HtmlFormat]  

exportersSplice :: MonadIO m => m [H.Node]
exportersSplice = return $ [H.Element "ul" [("id", "exportmenu")] (map menuItem (zip [0..] exporters))]
  where
    menuItem :: (Int, Exporter) -> H.Node
    menuItem (idx, Exporter exp) = H.Element "li" [("data-index", T.pack $ show idx )] [ H.TextNode $ T.pack $ displayName exp ]


handleExport :: AppHandler ()
handleExport = trace "HANDLE_EXPORT" $ do
    dh <- getDocumentHost
    doc <- accessAs dh Author
    maybeArgs <- getParam "args"
    maybe suppressError callExporter (maybeArgs >>= maybeReadBS >>= maybeExporter)
  where
    maybeReadBS = maybeRead . bsToStr
    maybeExporter = listToMaybe . flip drop exporters
    callExporter exp = do --writeBS "/static/js/common/docsnap.js"
      path <- liftIO $ writeToRandomFile "download" ":) victory"
      writeBS $ strToBs path
--      modifyResponse $ setContentType "application/octet-stream"
--      putResponse emptyResponse
--      modifyResponse $ addHeader "Content-Type" "application/octet-stream"
--      modifyResponse $ addHeader "Content-Disposition" "attachment; filename=\"fname.txt\""
--      modifyResponse $ addHeader "Content-Length" "3"
--      writeBS "asd"-}
--      writeBS $ "blablabla"



writeToRandomFile :: FilePath -> String -> IO FilePath
writeToRandomFile subdir content = do
    digits <- generateRandomDigits
    path <- searchUniqueFile subdir digits
    writeFile path content
    return path
  where
    searchUniqueFile subdir digits = do
        let randomPath = combine subdir $ take 10 digits
        exists <- doesFileExist randomPath
        if exists then searchUniqueFile subdir $ tail digits
                  else return randomPath
        
generateRandomDigits :: IO [Char]
generateRandomDigits = evalRandIO (getRandomRs (48,57)) >>= return . map chr

handleInitialCheckout :: AppHandler ()
handleInitialCheckout = trace "HANDLE_INITIAL_CHECKOUT_" $ do
    dh <- getDocumentHost
    doc <- accessAs dh Author
    revs <- getRevisions doc
    let curRev = maybe (Revision 0 []) id $ seqMergeRevisions revs
    logDSS $ show curRev  
    writeBS $ serialize $ Response curRev [] (-1)
    logDSS "initial checkout handled"
   

showCreateNewDialog :: AppHandler ()
showCreateNewDialog = renderWithSplices "main" [("heistscripts", openNewDialogSplice)]
  where
    openNewDialogSplice = scriptsSplice "static/js/newdlg/" "/static/js/"

handleIndex :: AppHandler ()
handleIndex = trace "HANDLE_INDEX" $ do
    showCreateNewDialog
  

lbsToBs :: BL.ByteString -> B.ByteString
lbsToBs = B.pack . BL.unpack

bsToLbs :: B.ByteString -> BL.ByteString
bsToLbs = BL.pack . B.unpack


handleCommand :: ByteString -> AppHandler ()
handleCommand cmd = trace ("HANDLE_COMMAND "++ bsToStr cmd) $ do
    case cmd of
      "init"   -> handleInitialCheckout
      "update" -> handleContentUpdate
      "share"  -> handleShare
      "export" -> handleExport
      _        -> pass

traceParams = getParams >>= \params -> logDSS $ "params=" ++ show params

  
-- | The application's routes.
routes :: [(ByteString, AppHandler ())]
routes = [ ("/", ifTop handleIndex)
         , ("/new", handleNew)
         , ("/:sk/",ifTop $ (getParam "cmd" >>= \p -> maybe pass handleCommand p) <|> handleOpen)
         , ("/download/", serveDirectory "download")
         , ("/static/", rlogDSS "tryServeStatic" >> serveDirectory "static" >> rlogDSS "ok")
         ]

rlogDSS :: String -> AppHandler ()
rlogDSS s = logDSS s

logHandlerStart = logDSSb '\n' "HANDLER STARTED"
logHandlerFinished = logDSSa '\n' "HANDLER FINISHED"

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    wrapSite (\site -> logHandlerStart >> traceParams >> site >> logHandlerFinished)
    h <- nestSnaplet "heist" heist $ heistInit' "templates" (mempty { hcLoadTimeSplices = defaultLoadTimeSplices })
    s <- nestSnaplet "sess" session $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    dh <- nestSnaplet "dochost" docHost $ documentHostInit
    addRoutes routes
    addSplices [("exporters", exportersSplice)]
    return $ App h s dh


  

