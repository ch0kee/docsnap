{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DocSnapServer where


import    Snap.Snaplet
import qualified Data.ByteString as B
import    Data.IORef

import Data.Aeson.TH
import qualified Data.Aeson as A

data DocSnapServer = DocSnapServer {
  dss_baseContent :: B.ByteString,
  dss_syncObject :: SyncObject
}

data Edit = Edit { edit_value :: String, edit_type :: String }
  deriving (Show) --atmenetileg

edit :: String -> String -> Edit
edit v t = Edit { edit_value=v, edit_type=t }

data SyncObject = SyncObject { so_diff :: [ Edit ] }
  deriving (Show)

$(deriveJSON (drop 5) ''Edit)
$(deriveJSON (drop 3) ''SyncObject)


--
dss_init :: DocSnapServer
dss_init = DocSnapServer { dss_baseContent = "", dss_syncObject = SyncObject { so_diff = [] } }
