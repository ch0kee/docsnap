{-# OPTIONS_HADDOCK ignore-exports #-}
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------------------
-- | Ez a backend html formátumú konverziót végzi
module DocSnap.Converter.Backends.HtmlConverter
  ( render
  , displayName
  , HtmlConverter(..)
  ) where
  
--------------------------------------------------------------------------------
import DocSnap.Converter
import DocSnap.Formatting
import  qualified Data.Text as T 
--------------------------------------------------------------------------------

-- | Nagyon leegyszerűsített html konverzió
data HtmlConverter = HtmlConverter
instance IConverter HtmlConverter
  where
    displayName = const "html file"
    render _ content = "<html><head>"++css++"</head><body>"++maindiv++"</body></html>"
      where
        maindiv = "<div style=\"white-space:pre\">"++content++"</div>"
        css = "<style type=\"text/css\">" ++ T.unpack formatCss ++ "</style>"

 
