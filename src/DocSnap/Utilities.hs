--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- | Ebben a modulban olyan segédeszközök találhatók, amelyek nem kapcsolódnak
-- szorosan az alkalmazáshoz, bármely Snap keretrendszert használó alkalmazás
-- használni tudja.
module DocSnap.Utilities
  ( getServerURL
  , isAjaxRequest
  , writeToRandomDir
  , locateMapKey
  ) where
  
--------------------------------------------------------------------------------
import qualified Data.Text as T
import           Snap
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import    System.FilePath (FilePath, combine)
import    System.IO (IO, putStrLn)
import    Data.Char (chr)
import           System.Directory
import           Control.Monad.Random (evalRandIO, getRandomRs)
import  Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.Map as M
import Debug.Trace
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- | Visszaadja a szerver böngészőben látható címét.
-- Előfordulhat, hogy hiányzik ez az adat, ekkor egy konstans értékkel
-- tér vissza. A paraméterben átadott elérési útvonalat az URI végére illeszti.
getServerURL :: (MonadSnap m)
             => T.Text  -- ^ Az URL végére illesztendő relatív útvonal  
             -> m T.Text
getServerURL path = do
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


--------------------------------------------------------------------------------
-- | Igazat ad vissza, ha az aktuális kérés JQuery ajaxon keresztül érkezett
isAjaxRequest :: (MonadSnap m)
              => m Bool
isAjaxRequest = do
    mb <- getsRequest (getHeader "X-Requested-With")
    case mb of
        Just b -> return ("XMLHttpRequest" == decodeUtf8 b)
        _ -> return False


--------------------------------------------------------------------------------
-- | szöveg írása a mappán belüli véletlen nevű almappába,
-- a megadott fájlnévvel
writeToRandomDir :: FilePath     -- ^ fájlnév
                  -> FilePath     -- ^ mappa 
                  -> String       -- ^ tartalom
                  -> IO FilePath
writeToRandomDir fname dir content = do
    unlessM (doesDirectoryExist dir) (createDirectory dir)
    digits <- generateRandomDigits
    randompath <- searchUniqueDir dir digits
    createDirectory randompath
    let filepath = combine randompath fname
    writeFile filepath content
    return filepath
  where
    searchUniqueDir :: FilePath -> FilePath -> IO FilePath
    searchUniqueDir dir digits = do
        let randomPath = combine dir $ take 10 digits
        ifM (doesDirectoryExist randomPath) (searchUniqueDir dir $ tail digits) (return randomPath)
    generateRandomDigits :: IO [Char]
    generateRandomDigits = evalRandIO (getRandomRs (48,57)) >>= return . map chr

-- | segédfüggvények
locateMapKey :: Eq a => a -> M.Map k a -> Maybe k
locateMapKey v m = locateMapKey' . dropWhile ((/=v) . snd)  $  M.toList m
  where
    locateMapKey' [] = Nothing
    locateMapKey' (x:_) = Just (fst x)
    
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM test actionTrue actionFalse = test >>= \t -> if t then actionTrue else actionFalse

whenM :: Monad m => m Bool -> m a -> m ()
whenM test actionTrue = ifM test (actionTrue >> return ()) $ return ()

unlessM :: Monad m => m Bool -> m a -> m ()
unlessM test actionFalse = ifM test (return ()) (actionFalse >> return ())

