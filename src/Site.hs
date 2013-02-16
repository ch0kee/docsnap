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
--import Text.JSON
--import Text.JSON.String

-- elso spliceom, kimasoltam
{-|
factSplice :: I.Splice Snap
factSplice = do
  input <- getParamNode
  let text = T.unpack $ X.nodeText input
      n = read text :: Int
  return [X.TextNode $ T.pack $ show $ product [1..n]]
  


------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = [("loginError", I.textSplice c) | c <- maybeToList authError]


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"
-}

-- tartalom kibontasa
fromMaybeContent :: Maybe ByteString -> ByteString
fromMaybeContent (Just bs) = bs
fromMaybeContent Nothing = B.empty

data Insert = Insert { index :: Int, content :: String }
  deriving (Show)

data SyncObject = SyncObject { inserts :: [Insert]  }
  deriving (Show)
$(deriveJSON id ''Insert)
-- $(deriveJSON id ''Remove)
$(deriveJSON id ''SyncObject)

-- Lekezeljuk az uj adatot
handleContentUpdate :: Handler App App ()
handleContentUpdate = method POST getter
  where
    getter = do
      ccp <- getParam "d" --get data
      scRef <- gets _cntnt --stored content reference 
      case ccp of
        Nothing -> updateClient scRef
        Just c  | (not.B.null) c -> storeContent scRef c
                | otherwise -> storeContent scRef ""
        
    updateClient scRef = do
      sc <- liftIO $ readIORef scRef --stored content
      writeBS sc
                 
    storeContent scRef c = do
      liftIO $ putStr $ show so
      liftIO $ modifyIORef' scRef (applyInserts (inserts so) )  --atomicModifyIORef kell
      sc <- liftIO $ readIORef scRef
      liftIO $ putStr "new value: "
      liftIO $ B.putStrLn sc
      updateClient scRef --itt ujra kiolvassuk, hatha szukseg van ra
        where so = fromJust $ syncObject c

modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f = do
    x <- readIORef ref
    let x' = f x
    x' `seq` writeIORef ref x'


applyInserts :: [Insert] -> ByteString -> ByteString
applyInserts is bs = foldl (\b i -> apply i b) bs is
  where apply (ins) b = B.concat [B.take idx b, strToBs cnt, B.drop idx b]
          where idx = index ins
                cnt = (content :: Insert -> String) ins

lbsToBs :: BL.ByteString -> B.ByteString
lbsToBs = B.pack . BL.unpack

bsToLbs :: B.ByteString -> BL.ByteString
bsToLbs = BL.pack . B.unpack

syncObject :: ByteString -> Maybe SyncObject
syncObject = A.decode . BL.pack . B.unpack     
--extractInserts :: ByteString -> Either String [String]
--extractInserts bs = (JS.runGetJSON JS.readJSObject str) 
--  where str = T.unpack . E.decodeASCII $ bs
--        eObj = JS.runGetJSON JS.readJSObject str
--        maybeInserts = case eObj of
--          Left  msg -> error msg
--          Right obj -> J.get_field obj 
--        insertArray = case maybeInserts of
--          Nothing -> []
--          Just is -> is

bsToStr :: ByteString -> String
bsToStr =  T.unpack . E.decodeUtf8      
          
strToBs :: String -> ByteString
strToBs =  E.encodeUtf8 . T.pack      

--        res = J.decode . T.unpack . E.decodeASCII $ bs

              
      --writeBS $ (E.encodeUtf8 . T.toLower . E.decodeUtf8) s

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/cupdate",  handleContentUpdate)
         , ("",          serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "users.json"
    --d <- nestSnaplet "dss" dss $ dssInit
    addRoutes routes
    addAuthSplices auth
    cref <- liftIO $ newIORef "" --kezdetben ures 
    --addSplices $ map (second liftHeist) [("fact",factSplice)]
    return $ App h s a cref

