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
import Control.Lens
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

-- Lekezeljuk az uj adatot
handleContentUpdate :: Handler App App ()
handleContentUpdate = method POST getter
  where
    getter = do
      maybeContributor <- with sessionLens $ getFromSession "contributor"
      case maybeContributor of
        Nothing -> logDSS "*INVALID CONTRIBUTOR"
        Just contrib -> do
          logDSS ("Contributor :" ++ show contrib)
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
        CommitSuccessful v -> do
          logDSS "COMMIT SUCCESSFUL"
          writeBS $ strToBs $ show v
          logDSS ("NEW VERSION [" ++ show v ++ "]")
        CheckoutOnly r -> do
          logDSS "CHECKOUT ONLY"
          writeBS $ strToBs $ serialize r
          logDSS ("CHECKED OUT [" ++ serialize r ++ "]")
      logDSS "<< END HANDLE COMMIT"

--csak azt kuldjuk vissza, amit a tobbiek csinaltak
{-
    updateClient dssRef = do
      liftIO $ putStrLn "*updating client"
      dss <- liftIO $ readIORef dssRef --stored content
      liftIO $ putStrLn $ (bsToStr . lbsToBs . A.encode) $ preservedSO (baseContentOfEditScript (dss_syncObject dss))
      writeSO $ preservedSO (baseContentOfEditScript (dss_syncObject dss))
      --writeBS $ (lbsToBs . A.encode) (dss_syncObject dss)
      liftIO $ putStrLn "*client updated"


    storeContent dssRef so = do
      liftIO $ putStr "*storing syncobject: "
      liftIO $ putStrLn $ show so--"storeContent"
      --liftIO . putStrLn . bsToStr . lbsToBs . A.encode $ SyncObject { so_diff=[ (edit "abc" "+"), (edit "xxx" "-")] }
      liftIO $ modifyIORef' dssRef (\dss -> dss { dss_syncObject=so }) --atomicModifyIORef kell
      --sc <- liftIO $ readIORef scRef
      --liftIO $ putStr "new value: "
      --liftIO $ B.putStrLn sc
      updateClient dssRef --itt ujra kiolvassuk, hatha szukseg van ra
      liftIO $ putStrLn "*syncobject stored"
-}

handleSayHello :: Handler App App ()
handleSayHello = method POST getter
  where
    getter = do
      liftIO $ putStrLn "*client is saying hello"
      with sessionLens $ setInSession "contributor" "0"
      with sessionLens $ commitSession
      --visszakuldjuk neki az osszes reviziot
      revs <- getRevisions
      liftIO $ putStrLn "begin serialize"
      liftIO $ putStrLn $ show (revs)
      liftIO $ putStrLn $ serialize (concatRevisions revs)
      liftIO $ putStrLn "end serialize"
      writeBS $ strToBs $ serialize (concatRevisions revs)

modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f = do
  x <- readIORef ref
  let x' = f x
  x' `seq` writeIORef ref x'

{-
applyInserts :: [Insert] -> ByteString -> ByteString
applyInserts is bs = foldl (\b i -> apply i b) bs is
  where apply (ins) b = B.concat [B.take idx b, strToBs cnt, B.drop idx b]
          where idx = (index :: Insert -> Int) ins
                cnt = (content :: Insert -> String) ins
-}

--preservedSO "" = EditScript []
--preservedSO s = EditScript { so_diff=[Edit {edit_value=s, edit_type="="}]}

--writeSO so = writeBS $ (lbsToBs . A.encode) so
--writeJSON rd = writeBS $ (lbsToBs . A.encode) rd

lbsToBs :: BL.ByteString -> B.ByteString
lbsToBs = B.pack . BL.unpack

bsToLbs :: B.ByteString -> BL.ByteString
bsToLbs = BL.pack . B.unpack

bsToStr :: ByteString -> String
bsToStr =  T.unpack . E.decodeUtf8

strToBs :: String -> ByteString
strToBs =  E.encodeUtf8 . T.pack

--        res = J.decode . T.unpack . E.decodeASCII $ bs


      --writeBS $ (E.encodeUtf8 . T.toLower . E.decodeUtf8) s

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("chello",   handleSayHello)
         , ("cupdate",  handleContentUpdate)
         , ("",          serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sessionLens $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sessionLens "users.json"
    rc <- nestSnaplet "revctrl" revLens $ revisionControlInit
    --d <- nestSnaplet "dss" dss $ dssInit
    addRoutes routes
    addAuthSplices auth
    --cref <- liftIO $ newIORef dss_init --kezdetben ures
    --addSplices $ map (second liftHeist) [("fact",factSplice)]
    return $ App h s a rc

