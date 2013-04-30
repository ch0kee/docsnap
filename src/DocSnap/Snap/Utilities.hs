--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- | Ebben a modulban olyan segédeszközök találhatók, amelyek nem kapcsolódnak
-- szorosan az alkalmazáshoz, bármely Snap keretrendszert használó alkalmazás
-- használni tudja.
module DocSnap.Snap.Utilities
  ( getServerURL
  ) where
  
--------------------------------------------------------------------------------
import qualified Data.Text as T
import           Snap
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
--------------------------------------------------------------------------------

import Debug.Trace
--------------------------------------------------------------------------------
-- | Visszaadja a szerver böngészőben látható címét.
-- Előfordulhat, hogy hiányzik ez az adat, ekkor egy konstans értékkel
-- tér vissza. A paraméterben átadott elérési útvonalat az URI végére illeszti.
getServerURL :: (MonadSnap m)
             => T.Text  -- ^ Az URL végére illesztendő relatív útvonal  
             -> m T.Text
getServerURL path = do
    req <- getRequest
    host <- originHeader
        >>= maybe (hostHeader
        >>= maybe dummyURL (return.decodeUtf8)) (return.decodeUtf8)
    return $ host `appendURL` path
  where
    originHeader = getsRequest (getHeader "Origin")
    hostHeader = getsRequest (getHeader "Host")
    dummyURL = return "<server-url>"
    appendURL :: T.Text -> T.Text -> T.Text
    appendURL first second = T.concat
      [ T.dropWhileEnd (=='/') first
      , "/"
      , T.dropWhile (=='/') second ]

