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


--data Insert = Insert { index :: Int, content :: String }
--  deriving (Show)

bsToStr :: ByteString -> String
bsToStr =  T.unpack . decodeUtf8

strToBs :: String -> ByteString
strToBs =  encodeUtf8 . T.pack


logDSSb c s = liftIO $ putStrLn (c:"** " ++ s)
logDSSa c s = liftIO $ putStrLn ("** " ++ s ++ [c])

-- $(deriveJSON id ''Insert)
-- $(deriveJSON id ''Remove)

--accessAsAuthor: megprobalja elerni a doksit a sütin és
--az url-en keresztül, ha nem megy megszakad az aktuális kezelő
--todo:authoraccess monad

--kulcs lekérése
getSharedKey :: AppHandler (Maybe SharedKey)
getSharedKey = do { msk <- getParam "sk"; return $ msk >>= Just . decodeUtf8 }

--visszaküld kezdőlapra hiba esetén
maybeDenied :: (a -> AppHandler b) -> Maybe a -> AppHandler b
maybeDenied = maybe (redirect "/")
--maybeDeniedWith $ return ()


--maybeDeniedWith :: AppHandler () -> (a -> AppHandler b) -> Maybe a -> AppHandler b
--maybeDeniedWith f = maybe $ f >> redirect "/"

accessAsAuthor :: AppHandler AuthorAccess
accessAsAuthor = getSharedKey >>= maybeDenied (\sk -> tryAccessAsAuthor sk >>= maybeDenied return)

--kitörli a sütiket is
--accessAsAuthor' :: AppHandler AuthorAccess
--accessAsAuthor' = getSharedKey >>= maybeDeniedWith f (\sk -> tryAccessAsAuthor sk >>= maybeDeniedWith f return)
--  where
--    f = (with session $ withSession session $ resetSession)
 
-- Lekezeljuk az uj adatot
handleContentUpdate :: AppHandler ()
handleContentUpdate = trace "HANDLE_CONTENT_UPDATE" $ do
  a <- accessAsAuthor
  getParam "args" >>= maybeDenied (\d -> trace "handle_commit" $ handleCommit a d)
  where
    handleCommit :: AuthorAccess -> B.ByteString -> AppHandler ()
    handleCommit a cdata = do
      --logDSS ("RECEIVED [" ++ bsToStr cdata ++ "]")
      response <- commit cdata a
      let respdata = serialize response
      --logDSS ("RESPONSE [" ++ respdata ++ "]")
      logDSS "*** NOW SENDING ***"
      writeBS $ strToBs $ respdata
      logDSS "*** SENT ***"
      
--   DJKFFSDKFKSDJF
--    Openben az url alapján kell nyitni, nem cookie alapján!

--hibás url esetén visszadobjuk a kezdőlapra és
--feldobunk egy dialogot a hibaüzenettel
--figyelve arra, hogy ha van sessionje, akkor azt ne töröljük
--jobb lenne, ha több tabon tudna dolgozni
handleOpen :: AppHandler ()
handleOpen = trace "HANDLE_OPEN" $ do
  accessAsAuthor
  loadExistingDocument
  getSharedKey >>= \sk -> maybeDenied storeSession sk
    where
      storeSession sk = with session $ do {resetSession ; setInSession "sk" sk; commitSession}
      loadExistingDocument = renderWithSplices "main"
        [ ("access_scripts", authorSplice)
        , ("sync_scripts", syncSplice) ]
      authorSplice = scriptsSplice "static/js/author/" "/static/js/"
      syncSplice = scriptsSplice "static/js/sync/" "/static/js/"

getAccessURI :: AuthorAccess -> ByteString
getAccessURI = encodeUtf8 . key

handleNew :: AppHandler ()
handleNew = trace "HANDLE_NEW" $ createDocument >>= redirect . getAccessURI



handleShare :: AppHandler ()
handleShare = trace "HANDLE_SHARE" $ return ()

handleInitialCheckout :: AppHandler ()
handleInitialCheckout = do
  logDSS "handling initial checkout"
  revs <- getRevisions =<< accessAsAuthor
  --logDSS $ show revs
  
  let curRev = concatRevisions revs
  logDSS $ show curRev
  d <- return . serialize $ maybe (Revision ([],0)) id curRev  
  logDSS "now sending"
  writeBS $ strToBs d
  logDSS "sent"
  --logDSS d
  logDSS "initial checkout handled"
  logDSS "handling initial checkout"
  where
   

showCreateNewDialog :: AppHandler ()
showCreateNewDialog = renderWithSplices "main" [("newdlg_script", openNewDialogSplice)]
  where
    openNewDialogSplice = scriptsSplice "static/js/newdlg/" "/static/js/"
  
  
liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

--ha van cookie és hozzáférhető akkor redirect oda,
--egyébként töröljük a sütit és showcreate
handleIndex :: AppHandler ()
handleIndex = trace "HANDLE_INDEX" $ do
  macc <- (with session $ getFromSession "sk") >>= maybe (return Nothing) (\sk -> tryAccessAsAuthor sk)
  case macc of
    Nothing  -> do
      with session $ withSession session $ resetSession
      showCreateNewDialog
    Just acc -> redirect $ getAccessURI acc
  return ()

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

traceParams = getParams >>= \params -> logDSS $ "params=" ++ show params

  
------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- | The application's routes.
--todo: gyökérben menjen az opendocument
routes :: [(ByteString, AppHandler ())]
routes = [ ("/", ifTop handleIndex)
--         , ("/:sk/", ifTop handleOpen)
         , ("/new", handleNew)
         , ("/:sk/",ifTop $ (getParam "cmd" >>= \p -> maybe pass handleCommand p) <|> handleOpen <|> writeBS "no handler")
         {-
                                , ("/init", handleInitialCheckout)
                                , ("/update",  handleContentUpdate)
                                , ("/new", handleNew)
                                , ("/share", handleShare)        
                                -} 
         --, ("/", with heist heistServe)
         , ("/static/", rlogDSS "tryServeStatic" >> serveDirectory "static" >> rlogDSS "ok")
        -- , ("", rlogDSS "tryRest" >> handleIndex >> rlogDSS "tryRest")
         ]

rlogDSS :: String -> AppHandler ()
rlogDSS s = logDSS s

logHandlerStart = logDSSb '\n' "HANDLER STARTED"
logHandlerFinished = logDSSa '\n' "HANDLER FINISHED"

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
  wrapSite (\site -> logHandlerStart >> traceParams >> site >> logHandlerFinished)
  --h <- nestSnaplet "heist" heist $ heistInit "templates"
  h <- nestSnaplet "heist" heist $ heistInit' "templates" (mempty { hcLoadTimeSplices = defaultLoadTimeSplices })
  s <- nestSnaplet "sess" session $
         initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  --rc <- nestSnaplet "revctrl" revLens $ revisionControlInit
  dh <- nestSnaplet "dochost" docHost $ documentHostInit
  addRoutes routes
  return $ App h s dh

