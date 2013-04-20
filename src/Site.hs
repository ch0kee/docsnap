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
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
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
import DocSnapServer

import qualified Text.XmlHtml as X
import          Data.IORef

import Control.Arrow hiding (app)

import Data.Aeson.TH
import qualified Data.Aeson as A
import Snap.Snaplet.Session

import Serialize
import Internal.Types


-- tartalom kibontasa
fromMaybeContent :: Maybe ByteString -> ByteString
fromMaybeContent (Just bs) = bs
fromMaybeContent Nothing = B.empty

--data Insert = Insert { index :: Int, content :: String }
--  deriving (Show)

logDSS = liftIO . putStrLn

-- $(deriveJSON id ''Insert)
-- $(deriveJSON id ''Remove)

--todo: handle permissions
handleOpen :: Handler App App ()
handleOpen = render "test"--writeBS "nothing yet"
{-
handleOpenDocument = do
  id <- getParam "id"
  maybe_access <- tryAccessDocument id
  case maybe_access of
    Nothing -> writeBS "did not find any doc :("
    Just access -> open access
    where
      open (_, doc) = 
-}

-- Lekezeljuk az uj adatot
handleContentUpdate :: Handler App App ()
handleContentUpdate = method POST getter
  where
    getter = do
      maybeContributor <- with sessionLens $ getFromSession "contributor"
      case maybeContributor of
        Nothing -> logDSS "*INVALID CONTRIBUTOR"
        Just contrib -> do
--          logDSS ("Contributor :" ++ show contrib)
          ccp <- getParam "d"
          case ccp of
            Nothing -> undefined --error
            Just c  | (not . B.null) c -> handleCommit c
                    | otherwise -> undefined --error

    handleCommit :: ByteString -> Handler App App ()
    handleCommit cdata = do
      logDSS ">> START HANDLE COMMIT"
      logDSS ("RECEIVED [" ++ bsToStr cdata ++ "]")
      response <- commit $ bsToStr cdata
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


handleNew :: Handler App App ()
handleNew = method GET getter
  where
    getter = do
      logDSS "creating new document"
      redirect' "/d/42" 303
      --writeBS "creating new document"
      --id <- generateNewID
      


handleShare :: Handler App App ()
handleShare = method POST getter
  where
    getter = do
      return ()

--redirect' OR redirect ?
handleSayHello :: Handler App App ()
handleSayHello = method POST getter
  where
    getter = do
      liftIO $ putStrLn "*START SAYING HELLO"
      --with sessionLens commitSession
      --doc <- createNewDocument
      --testdoc <- getTestDocument
      --visszakuldjuk neki az osszes reviziot
      revs <- getRevisions
      --liftIO $ putStrLn $ show revs
      --liftIO $ putStrLn $ serialize (concatRevisions revs)
      writeBS $ strToBs $ serialize (concatRevisions revs)
      liftIO $ putStrLn "*END SAYING HELLO"

showCreateNewDialog :: Handler App App ()
showCreateNewDialog = renderWithSplices "main" [("newdialog", newDialogSplice)]
  where
    newDialogSplice :: Monad m => I.Splice m
    newDialogSplice = liftM (maybe [] id) $ I.evalTemplate "_new_dialog"

loadExistingDocument :: Handler App App ()
loadExistingDocument = undefined
  --heistLocal (bindSplice newDialogSplice) $ render "main"
  

handleInit :: Handler App App ()
handleInit = do
  maybe_savedid <- withSession sessionLens $ with sessionLens $ getFromSession "id"
  case maybe_savedid of
    Nothing -> do
      liftIO $ putStrLn "No saved session"
      showCreateNewDialog
--      withSession sessionLens $ with sessionLens $ setInSession "id" "42"
    Just savedid -> do
      liftIO $ putStrLn "Saved session found"
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

bsToStr :: ByteString -> String
bsToStr =  T.unpack . E.decodeUtf8

strToBs :: String -> ByteString
strToBs =  E.encodeUtf8 . T.pack



------------------------------------------------------------------------------
-- | The application's routes.
--todo: gyökérben menjen az opendocument
routes :: [(ByteString, Handler App App ())]
routes = [ ("/", ifTop $ handleInit)
         , ("/chello",   handleSayHello)
         , ("/cupdate",  handleContentUpdate)
         , ("/cnew", handleNew)
         , ("/cshare", handleShare)
         , ("/d/:id", handleOpen)
         , ("/", with heist heistServe)
         , ("/", serveDirectory "static")
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
  addRoutes routes
  h <- nestSnaplet "heist" heist $ heistInit "templates"
  s <- nestSnaplet "sess" sessionLens $
         initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  --rc <- nestSnaplet "revctrl" revLens $ revisionControlInit
  dh <- nestSnaplet "dochost" docHostLens $ documentHostInit
  return $ App h s dh

