{-# OPTIONS_HADDOCK ignore-exports #-}
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------------------
-- | Ez a backend egyszerű szöveges formátumba exportálja az adatot 
module DocSnap.Converter.Backends.TxtConverter
  ( render
  , displayName
  , TxtConverter(..)  
  ) where
  
--------------------------------------------------------------------------------
import Text.HTML.TagSoup (canonicalizeTags, parseTags, Tag(..))
--------------------------------------------------------------------------------
import DocSnap.Converter
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- | TxtConverter konstruktor
data TxtConverter = TxtConverter
instance IConverter TxtConverter
  where
    displayName = const "text file"
    render _ = concat . map substitute . canonicalizeTags . parseTags


--------------------------------------------------------------------------------
-- | Leírja a (Html elem)->(Karaktersorozat) helyettesítési szabályokat,
-- amelyeket az átalakító használ.
substitute :: Tag String -> String
substitute (TagOpen "div" _) = ""
substitute (TagOpen "br" _ ) = "\n"
substitute (TagClose "div" ) = "\n"
substitute (TagText txt ) = txt
substitute _ = ""

