{-# LANGUAGE GADTs #-}
module Main where

import Lib
import System.IO
import Data.Char (isSpace, isSeparator)

import Control.Applicative hiding (many)
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State, (<*))
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug(dbg)

-- 我々のYAMLはScalarに改行を許しません
-- type Key = Text
-- data Yaml = Scalar Text
--           | Sequence [Yaml]
--           | Mapping (Key, Yaml)
--           | Comment Text
--           | Block [Yaml]
--   deriving (Eq, Show)

newtype Key = Key Text deriving (Eq, Show)
newtype Scalar = Scalar Text deriving (Eq, Show)
data Sentence where
  Sequence  :: [Yaml] -> Sentence
  Mapping   :: Key -> Yaml -> Sentence
  Comment   :: Text -> Sentence
    deriving (Eq, Show)

data Yaml where
  Literal :: Scalar -> Yaml
  Block   :: [Sentence] -> Yaml
    deriving (Eq, Show)

type Parser = Parsec Void Text

isLiteral :: Char -> Bool
isLiteral c = c `notElem` [' ', '\n']

newline' :: Parser ()
newline' = dbg "N" (void (char '\n') <|> void newline) <|> eof

litStr :: Parser Text
litStr = takeWhile1P (Just "litStr") isLiteral

pKey :: Parser Key
pKey = Key <$> takeWhile1P (Just "Key") (/= ':')

pYaml :: Parser Yaml
pYaml = pBlock <|> pLiteral
-- pYaml = dbg "yaml" pScalar

-- FIXME: parseTest pBlock "abc: def   \ncde: efg"
-- FIXME: なにもわからない(eofで止まらない)
pBlock :: Parser Yaml
pBlock = Block <$> many (dbg "Mapping" pMapping')
  where
    space' = takeWhileP Nothing (== ' ')
    pMapping' = pMapping <* space' <* newline'

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
