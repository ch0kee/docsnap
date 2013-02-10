{-# LANGUAGE OverloadedStrings #-}

module DocSnapServer where


import    Snap.Snaplet
import qualified Data.ByteString as B
import    Data.IORef

--data DocSnapServer = DocSnapServer {
--  _content :: IO(IORef B.ByteString)
--}
--
--dssInit :: SnapletInit b DocSnapServer
--dssInit = makeSnaplet "dss" "DocSnapServer snaplet" Nothing $ do
--  return DocSnapServer { _content = newIORef "<empty>" }
