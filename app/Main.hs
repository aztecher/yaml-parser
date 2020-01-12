{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Lib
import System.IO
import Data.Char (isSpace, isSeparator)

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State, (<*))
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug(dbg)

newtype Key = Key Text deriving (Eq, Show)
newtype Scalar = Scalar Text deriving (Eq, Show)
data Sentence where
  Sequence  :: [Yaml] -> Sentence
  Mapping   :: Key -> Yaml -> Sentence
    deriving (Eq, Show)

data Yaml where
  Literal :: Scalar -> Yaml
  Block   :: [Sentence] -> Yaml
    deriving (Eq, Show)

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn :: Parser ()
scn = L.space space1 lineComment empty

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc



---------------------------------------------------------------------------------------------------

isLiteral :: Char -> Bool
isLiteral c = c `notElem` [' ', '\n']

litStr :: Parser Text
litStr = lexeme $ takeWhile1P (Just "litStr") isLiteral

pKey :: Parser Key
pKey = Key <$> takeWhile1P (Just "Key") (/= ':')

pYaml :: Parser Yaml
pYaml =  pLiteral <|> pBlock
-- pYaml = dbg "yaml" pScalar

-- FIXME: parsetest pblock "abc: def   \ncde: efg"
-- FIXME: なにもわからない(eofで止まらない)
pBlock :: Parser Yaml
pBlock = Block <$> many (dbg "Mapping" pMapping')
  where
    space' = takeWhileP Nothing (== ' ')

top :: Parser Yaml
top = L.nonIndented scn p
  where
    p = do
      key <- pKey
      char ':'
      return $ noPattern <|> somePattern <|>  manyPattern
        where
          noPattern = lexeme pLiteral
          somePattern = return (L.IndentSome Nothing (pure . Block) pMapping) -- これが問題
          manyPattern = undefined

pMapping :: Parser Sentence
pMapping = do
  key <- pKey
  char ':'
  space
  yaml <- pYaml
  return $ Mapping key yaml

pScalar :: Parser Scalar
pScalar = Scalar <$> litStr

pLiteral :: Parser Yaml
pLiteral = Literal <$> pScalar

pSentence :: Parser Sentence
pSentence = pMapping

main :: IO ()
main = do
  withFile "package.yaml" ReadMode $ \handle -> do
    str <- hGetContents handle
    putStr . show $ str
