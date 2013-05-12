{-# OPTIONS_HADDOCK ignore-exports #-}
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------------------
-- | Ez a backend html formátumban, azaz egy az egyben visszaadja a szöveget 
module DocSnap.Converter.Backends.HtmlConverter
  ( render
  , displayName
  , HtmlConverter(..)
  ) where
  
--------------------------------------------------------------------------------
import DocSnap.Converter
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | HtmlConverter konstruktor
data HtmlConverter = HtmlConverter
instance IConverter HtmlConverter
  where
    displayName = const "html file"
    render _ = id


