-----------------------------------------------------------------------------
--
-- Module      :  Serialize
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :  csokkerfalva@gmail.com
-- Stability   :  Experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Serialize (
  parseEditScript,
  parseRevision
) where


import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
--import qualified Text.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language (emptyDef)

import Internal.Types

import Control.Monad (replicateM, mapM)
-- [=14|+3:alm|-2]

lexer = P.makeTokenParser emptyDef
natural = P.natural lexer

--majd az egymás utáni addokat meg stb-t össze kellene vonni
serialize :: Revision -> String
serialize (Revision (es, v)) = (show v) ++ ('[':(serialize' es)++"]")
  where
    serialize' :: [Edit] -> String
    serialize' [] = ""
    serialize' (Insert c:rest) = "
    serialize' (Insert c:rest) = "
    serialize' (Insert c:rest) = "

parseEditScript :: String -> Either ParseError [Edit]
parseEditScript input = parse editscript "error" input

parseRevision :: String -> Either ParseError Revision
parseRevision input = parse revision "error" input

editscript :: CharParser () [Edit]
editscript = do
  char '['
  edits <- sepBy edit (char '|')
  char ']'
  return $ concat edits

revision :: CharParser () Revision
revision = do
  version <- natural
  edits <- editscript
  return $ Revision (edits, fromInteger version)

edit :: CharParser () [Edit]
edit =  try insert --insert
    <|> try remove --remove
    <|> try preserve --preserve

insert :: CharParser () [Edit]
insert = do
  char '+'
  n <- natural
  ins <- count (fromInteger n) anyChar
  mapM (\c -> return $ Insert c) ins

remove :: CharParser () [Edit]
remove = do
  char '-'
  n <- natural
  replicateM (fromInteger n) $ return Remove

preserve :: CharParser () [Edit]
preserve = do
  char '='
  n <- natural
  replicateM (fromInteger n) $ return Preserve



