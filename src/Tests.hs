{-# OPTIONS_HADDOCK ignore-exports #-}
--------------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
--------------------------------------------------------------------------------
-- | Teszteket tartalmaz
module Tests where

--------------------------------------------------------------------------------
--import Test.QuickCheck
--------------------------------------------------------------------------------
import DocSnap.Serialize
import DocSnap.Repository
import DocSnap.Export
import Data.Maybe

deriving instance Eq Request
deriving instance Eq Revision
deriving instance Eq PackedEdit

testRevision1 = (Revision 42 [I "alma", R 14, P 5])
testRevision2 = (Revision 0 [])
testRevision3 = (Revision 12 [R 14, P 5]) 

 
testRequest1 = Request testRevision1 "name" ["ch1","ch2"] 123
testRequest2 = Request testRevision2 "name" ["ch2"] 123
testRequest3 = Request testRevision3 "name" [] 123

testSerialize req = req == (fromJust . deserialize . serialize) req

testDocument1 = Document [] [(0, ChatMessage "sender1" "msg1"), (1, ChatMessage "sender2" "msg2")]
testDocument2 = Document [] []

--testSendChatMessage doc msg = sendChatMessage doc ==  
