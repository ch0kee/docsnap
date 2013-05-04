{-# OPTIONS_HADDOCK ignore-exports #-}
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------------------
-- | Ez a backend html formátumban, azaz egy az egyben visszaadja a szöveget 
module DocSnap.Export.Converter.Backends.HtmlConverter
  ( render
  , displayName
  , HtmlConverter(..)
  ) where
  
--------------------------------------------------------------------------------
import DocSnap.Export.Converter
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | HtmlConverter konstruktor
data HtmlConverter = HtmlConverter
instance IConverter HtmlConverter
  where
    displayName = const "html file"
    render _ = id


--------------------------------------------------------------------------------
-- | tesztadatok
testdata = "Ez itt az első sor<div><br></div><div>Igazából ez lett a második sor</div><div><br><div>Ez pedig itt már a második sor</div></div><div><br></div><div><br></div><div>Csináltam ezelé két üres sort, hogy jobban látszódjon</div>"
testdata2 = "Ez itt az első sor<div><br></div><div>Igazából ez lett a második sor</div><div><br><div>Ez pedig itt már a második sor</div></div><div><br></div><div><br></div><div>Csináltam ezelé két üres sort, hogy jobban látszódjon<div>"
testdata3 = "dff DSF SDF SDF D<br><br>dsadasd<br><br><div>sd fsd fsdfsd f D</div><div><br></div><div>dasdS<br>DADASDAS<br><br></div>"

