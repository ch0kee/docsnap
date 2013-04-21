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
import qualified Data.Text.Encoding as E
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

-- tartalom kibontasa
fromMaybeContent :: Maybe ByteString -> ByteString
fromMaybeContent (Just bs) = bs
fromMaybeContent Nothing = B.empty

--data Insert = Insert { index :: Int, content :: String }
--  deriving (Show)

bsToStr :: ByteString -> String
bsToStr =  T.unpack . E.decodeUtf8

strToBs :: String -> ByteString
strToBs =  E.encodeUtf8 . T.pack

logDSS s = liftIO $ putStrLn ("** " ++ s)
logDSSb c s = liftIO $ putStrLn (c:"** " ++ s)
logDSSa c s = liftIO $ putStrLn ("** " ++ s ++ [c])

-- $(deriveJSON id ''Insert)
-- $(deriveJSON id ''Remove)

--accessAsAuthor: megprobalja elerni a doksit a sütin és
--az url-en keresztül, ha nem megy megszakad az aktuális kezelő
accessAsAuthor :: Handler App App AuthorAccess
accessAsAuthor = do
  macc <- do
    msk <- with session $ getFromSession "sk"
    case msk of
      Nothing -> return Nothing 
      Just sk -> tryAccessAsAuthor . T.unpack $ sk
    
  case macc of
    Nothing -> do
      with session $ withSession session $ resetSession --vagy nincs süti, vagy nincs doksi, mindenkepp rossz a süti
      --probaljuk meg az URL-bol kibanyaszni :/
      r <- getRequest
      let uri = rqURI r
      logDSS ("looking for sk rqURI:" ++ bsToStr uri) --talán rqPathInfo-ban jobb adat van
      macc' <- tryAccessAsAuthor . T.unpack . T.takeWhile (/='/') . T.dropWhile (=='/') . E.decodeUtf8 $ uri
      case macc' of
        Nothing -> do --nincs mit tenni, kuka minden, esetleg dobhatunk egy hibaüzenetet
          redirect "/"
        Just acc -> do--valószínüleg kikapcsolta a sütiket, áttérünk URL írásra
          --with session $ withSession session $ setInSession "sk" (T.pack $ key acc) --próbáljuk megint letárolni
          return acc
    Just acc -> do --ok, a sütiben jó adat van
      with session $ withSession session $ touchSession --kell commit ehhez?
      return acc
  
-- Lekezeljuk az uj adatot
handleContentUpdate :: Handler App App ()
handleContentUpdate = method POST getter
  where
    getter = do
      logDSS "HANDLE_CONTENT_UPDATE"
      ccp <- getParam "d"
      case ccp of
        Nothing -> undefined --error
        Just c  | (not . B.null) c -> handleCommit c
                | otherwise -> undefined --error

    handleCommit :: ByteString -> Handler App App ()
    handleCommit cdata = do
      logDSS ">> START HANDLE COMMIT"
      logDSS ("RECEIVED [" ++ bsToStr cdata ++ "]")
      a <- accessAsAuthor
      response <- commit (bsToStr cdata) a
      case response of
        CommitSuccessful v -> do --
          writeBS $ strToBs $ 'd':(show v)
          logDSS ("NEW VERSION [" ++ 'd':(show v) ++ "]")
        CheckoutOnly r -> do --
          writeBS $ strToBs $ 'o':(serialize r)
          logDSS ("CHECKED OUT [" ++ 'o':(serialize r) ++ "]")
        NoChanges -> do
          writeBS $ strToBs $ "n"
          logDSS ("NO CHANGES")
      logDSS "<< END HANDLE COMMIT"
      
--   DJKFFSDKFKSDJF
--    Openben az url alapján kell nyitni, nem cookie alapján!

--hibás url esetén visszadobjuk a kezdőlapra és
--feldobunk egy dialogot a hibaüzenettel
--figyelve arra, hogy ha van sessionje, akkor azt ne töröljük
--jobb lenne, ha több tabon tudna dolgozni
handleOpen :: Handler App App ()
handleOpen = do
  logDSS "HANDLE_OPEN"
  sk <- getParam "sk"
  --params <- getParams
  --logDSS $ "params=" ++ (show params)
  accessAsAuthor
  loadExistingDocument
    where
      loadExistingDocument = renderWithSplices "main" [("templatescripts", authorSplice)]
      authorSplice = scriptsSplice "static/js/author/" "/static/js/"

getAccessURI :: SharedKey -> String
getAccessURI = ("/"++) 

handleNew :: Handler App App ()
handleNew = do
  logDSS "HANDLE_NEW"
  sk <- createDocument
  let url = getAccessURI sk
  with session $ do
    resetSession
    setInSession "sk" (T.pack sk)
    commitSession
  redirect (strToBs url)


handleShare :: Handler App App ()
handleShare = method POST getter
  where
    getter = do
      logDSS "HANDLE_SHARE"
      return ()

handleInitialCheckout :: Handler App App ()
handleInitialCheckout = method POST getter
  where
    getter = do
      logDSS "HANDLE_INITIAL_CHECKOUT"
      a <- accessAsAuthor
      logDSS "initial checkout"
      revs <- getRevisions a
      logDSS ("pack " ++ show (length revs) ++ " revisions")
      writeBS $ strToBs $ serialize (concatRevisions revs)

showCreateNewDialog :: Handler App App ()
showCreateNewDialog = renderWithSplices "main" [("templatescripts", openNewDialogSplice)]
  where
    openNewDialogSplice = scriptsSplice "static/js/newdlg/" "/static/js/"
  

handleIndex :: Handler App App ()
handleIndex = do
  logDSS "HANDLE_INDEX"
  msk <- with session $ getFromSession "sk"
  case msk of
    Nothing -> do
      logDSS "new session"
      showCreateNewDialog
--      withSession sessionLens $ with sessionLens $ setInSession "id" "42"
    Just sk -> do
      logDSS ("saved session: " ++ T.unpack sk)
      redirect . strToBs . T.unpack $ sk
  --ifTop $ render "main"

modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f = do
  x <- readIORef ref
  let x' = f x
  x' `seq` writeIORef ref x'

lbsToBs :: BL.ByteString -> B.ByteString
lbsToBs = B.pack . BL.unpack

bsToLbs :: B.ByteString -> BL.ByteString
bsToLbs = BL.pack . B.unpack


------------------------------------------------------------------------------
-- | The application's routes.
--todo: gyökérben menjen az opendocument
routes :: [(ByteString, Handler App App ())]
routes = [ ("/", ifTop handleIndex)
         , ("/:sk", ifTop handleOpen)
         , ("/cmd/init", handleInitialCheckout)
         , ("/cmd/update",  handleContentUpdate)
         , ("/cmd/new", handleNew)
         , ("/cmd/share", handleShare)         
         --, ("/", with heist heistServe)
         , ("/static/", rlogDSS "tryServeStatic" >> serveDirectory "static" >> rlogDSS "ok")
        -- , ("", rlogDSS "tryRest" >> handleIndex >> rlogDSS "tryRest")
         ]

rlogDSS :: String -> Handler App App ()
rlogDSS s = logDSS s
{-
p = do { r <- getRequest; logDSS ("rqURI:" ++ (bsToStr . rqURI) r) }
c s = logDSS s >> writeBS (strToBs s)
--srv = ("", logDSS "serveDirectory" >> serveDirectory "static")
t1 = ("/", rlogDSS "try1" >> ifTop (c "elso") >> rlogDSS "ok1")
t2 = ("/:sk", rlogDSS "try2" >> ifTop (c "capture") >> rlogDSS "ok2")
t3 = ("/", rlogDSS "try3" >> c "harmadik" >> rlogDSS "ok3")
t4 = ("/", rlogDSS "try4" >> ifTop (c "negyedik") >> rlogDSS "ok4")
t5 = ("/", rlogDSS "try5" >> c "otodik" >> rlogDSS "ok5")
testroutes = [ t2, t1 ]--, t5,("", do { r <- getRequest; writeBS "not matched"; writeBS (rqURI r) }) ]
-}

logHandlerStart = logDSSb '\n' "HANDLER STARTED"
logHandlerFinished = logDSSa '\n' "HANDLER FINISHED"

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
  wrapSite (\site -> logHandlerStart >> site >> logHandlerFinished)
  --h <- nestSnaplet "heist" heist $ heistInit "templates"
  h <- nestSnaplet "heist" heist $ heistInit' "templates" (mempty { hcLoadTimeSplices = defaultLoadTimeSplices })
  s <- nestSnaplet "sess" session $
         initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  --rc <- nestSnaplet "revctrl" revLens $ revisionControlInit
  dh <- nestSnaplet "dochost" docHostLens $ documentHostInit
  addRoutes routes
  return $ App h s dh

