{-# LANGUAGE ExistentialQuantification #-}

module Exporter where

data Exporter = forall a. ExportableFormat a => Exporter a

-- exporter
class ExportableFormat a
  where
    displayName :: a -> String
    convert :: a -> String -> String
