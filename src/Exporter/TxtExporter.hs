{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Exporter.TxtExporter where

import Exporter
import Text.HTML.TagSoup

data TxtFormat = TxtFormat
instance ExportableFormat TxtFormat
  where
    displayName = const "text file"
    convert _ raw = concat . map substitute . canonicalizeTags . parseTags $ raw

convert :: String -> String
convert = concat . map substitute . canonicalizeTags . parseTags

substitute :: Tag String -> String
substitute (TagOpen "div" _) = ""
substitute (TagOpen "br" _ ) = "\n"
substitute (TagClose "div" ) = "\n"
substitute (TagText txt ) = txt
substitute _ = ""

--saveTestToFile :: String -> IO ()
--saveTestToFile td = writeFile "test.txt" $ convert td

testdata = "Ez itt az első sor<div><br></div><div>Igazából ez lett a második sor</div><div><br><div>Ez pedig itt már a második sor</div></div><div><br></div><div><br></div><div>Csináltam ezelé két üres sort, hogy jobban látszódjon</div>"

testdata2 = "Ez itt az első sor<div><br></div><div>Igazából ez lett a második sor</div><div><br><div>Ez pedig itt már a második sor</div></div><div><br></div><div><br></div><div>Csináltam ezelé két üres sort, hogy jobban látszódjon<div>"

testdata3 = "dff DSF SDF SDF D<br><br>dsadasd<br><br><div>sd fsd fsdfsd f D</div><div><br></div><div>dasdS<br>DADASDAS<br><br></div>"
