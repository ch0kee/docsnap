
--------------------------------------------------------------------------------
-- | Itt találhatók mindenféle segédeszközök (pontosítani!
module DocSnap.Internal.Utilities where

import    System.FilePath (FilePath, combine)
import    System.IO (IO, putStrLn)
import    Data.Char (chr)
import           System.Directory (doesFileExist)
import           Control.Monad.Random (evalRandIO, getRandomRs)
import  Control.Monad.Trans (liftIO, MonadIO)
import qualified Data.Map as M
--------------------------------------------------------------------------------
-- | Tartalom exportálása véletlen nevű fájlba.
-- dir: könyvtárba, content: ezt írjuk bele, visszatér a létrejött fájllal
writeToRandomFile :: FilePath -> String -> IO FilePath
writeToRandomFile dir content = do
    digits <- generateRandomDigits
    path <- searchUniqueFile dir digits
    writeFile path content
    return path
  where
    searchUniqueFile :: FilePath -> FilePath -> IO FilePath
    searchUniqueFile dir digits = do
        let randomPath = combine dir $ take 10 digits
        exists <- doesFileExist randomPath
        if exists then searchUniqueFile dir $ tail digits
                  else return randomPath
    generateRandomDigits :: IO [Char]
    generateRandomDigits = evalRandIO (getRandomRs (48,57)) >>= return . map chr


locateMapKey :: Eq a => a -> M.Map k a -> Maybe k
locateMapKey v m = locateMapKey' . dropWhile ((/=v) . snd)  $  M.toList m
  where
    locateMapKey' [] = Nothing
    locateMapKey' (x:_) = Just (fst x)
    
logDSS :: (MonadIO m) => String -> m ()
logDSS s = liftIO $ putStrLn ("** " ++ s)

rlogDSS :: (MonadIO m) => String -> m ()
rlogDSS s = logDSS s
    
