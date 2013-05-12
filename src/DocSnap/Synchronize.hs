{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}

module DocSnap.Synchronize where

import Snap
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.State
import qualified Data.Map as M 
import qualified Data.Aeson as A
import  Data.Aeson.TH
import Debug.Trace
import DocSnap.Serialize
import Data.Maybe
import qualified Data.ByteString as B
import qualified Data.Text as T
import    Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Control.Monad.Trans.Maybe

initAjaxSynchronize :: SnapletInit b (AjaxSynchronize a)
initAjaxSynchronize = makeSnaplet "sync" "Synchronize Snaplet" Nothing $ do
    return $ AjaxSynchronize Nothing
    
--ez akkor Just, ha be van cache-elve
newtype AjaxSynchronize a = AjaxSynchronize {
  payloadMap :: Maybe (M.Map String B.ByteString) --cache
}
    
-- | Visszatér a megadott értékkel, vagy hibaüzenet küld
receive :: (Show d, A.FromJSON a)
        => d -> Handler b (AjaxSynchronize d) a
receive module_ = do
     
    pm <- loadPayloads
    maybe haltWithAjaxError return $ do
      bs <- M.lookup (show module_) pm
      deserialize bs
  
send :: (Show d, A.ToJSON a)
        => d -> a -> Handler b (AjaxSynchronize d) ()
send module_ what = do
    pm <- loadPayloads
    put (AjaxSynchronize $ Just $ M.insert (show module_) (serialize what) pm)

storePayloads :: Handler b (AjaxSynchronize a) ()
storePayloads = do
    gets payloadMap >>= maybe haltWithAjaxError store'
  where
    store' ps = do
      let pkgs = Packages $ map (\(k,v) -> ModulePackage k v) $ M.toList ps
      putResponse emptyResponse
      writeBS $ serialize pkgs

loadPayloads :: Handler b (AjaxSynchronize a) (M.Map String B.ByteString)
loadPayloads = do
    gets payloadMap >>=
      (`maybe` return) (extractParams >>= maybe haltWithAjaxError getCached)
  where
    extractParams = maybe Nothing deserialize `liftM` getParam "args"   
    getCached (Packages pkgs) = do
        let pm = M.fromList $ map (\(ModulePackage s b) -> (s,b)) pkgs
        put $ AjaxSynchronize $ Just pm
        return $ pm
  

-- | Moduloknak szánt üzenetek
data Packages = Packages { packages :: [ModulePackage] }
  
-- | Modul üzenete
data ModulePackage = ModulePackage
  { pkg_module :: String
  , pkg_payload :: B.ByteString
  }
  
  --------------------------------------------------------------------------------
-- | Ismeretlen hiba jelzése AJAX kérés esetén
respondUnknownAjaxError :: (MonadSnap m) => m ()
respondUnknownAjaxError = writeBS . serialize $ ErrorAjaxResponse "The service encountered an unknown error."

haltWithAjaxError :: Handler b (AjaxSynchronize d) a
haltWithAjaxError = respondUnknownAjaxError >> getResponse >>= finishWith

data ErrorAjaxResponse = ErrorAjaxResponse
    { errorAjaxResponse_error :: String }
    

$(deriveJSON (drop 4) ''ModulePackage)
$(deriveJSON id ''Packages)
$(deriveJSON (drop 18) ''ErrorAjaxResponse)

