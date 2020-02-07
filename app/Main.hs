{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase        #-}
module MegaparsecSample.Sample where
import           Control.Applicative        hiding (many)
import           Control.Monad
import           Data.Char                  (isSeparator, isSpace, isAlpha)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Void
import           System.IO
import           Text.Megaparsec            hiding (State, some, (<*))
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Debug      (dbg)
import           Text.Pretty.Simple

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

------------------------------------------------------------------------------------------------
isLiteral :: Char -> Bool
isLiteral c = c `notElem` [' ', '\n']

litStr :: Parser Text
litStr = lexeme $ try quoted <|> takeWhile1P (Just "litStr") isLiteral
  where
    quoted = do
      void (char '"')
      str <- takeWhileP (Just "quoted") (/= '"')
      void (char '"')
      pure str

 -- TODO: fix: '-'から始まるkeyが取得できてしまう
pKey :: Parser Key
pKey = dbg "pKey" $ lexeme $ Key <$> key'
  where
    key' = T.cons <$> (satisfy isAlpha) <*> (takeWhile1P (Just "Key") (`notElem` (":" :: String)))

pYaml :: Parser Yaml
pYaml =  lexeme $ pLiteral <|> pBlock

-- pYaml :: Parser Yaml
-- pYaml = try noPattern <|> try somePattern <|>  try manyPattern
--           where
--             singleton :: Key -> Yaml -> Yaml
--             singleton key = (Block . (\x -> [x]) . Mapping key)
--             noPattern = do
--               key <- pKey
--               pColon
--               dbg "noPattern" $ singleton key <$> lexeme pLiteral
--             somePattern = dbg "somePattern" $ L.indentBlock scn $ do
--               key <- pKey
--               pColon
--               return (L.IndentSome Nothing (pure . singleton key . Block) pMapping)
--             manyPattern = dbg "manyPattern" $ L.indentBlock scn $ do
--               key <- pKey
--               pColon
--               return (L.IndentMany Nothing (pure . singleton key . Block . (\x -> [x]) . Sequence) pOneItem)

pBlock :: Parser Yaml
pBlock = Block <$> many (dbg "Mapping" pMapping)


-- top :: Parser Yaml
-- top = go
--   where
--     go = L.nonIndented scn (dbg "allPattern" p) >>= \case
--       l@Literal _ -> pure l
--       Block
--         where
--           singleton :: Key -> Yaml -> Yaml
--           singleton key = (Block . (\x -> [x]) . Mapping key)
--           p :: Parser Yaml
--           p = try noPattern <|> try somePattern <|>  try manyPattern
--               where
--                 noPattern = do
--                   key <- pKey
--                   pColon
--                   dbg "noPattern" $ singleton key <$> lexeme pLiteral
--                 somePattern = dbg "somePattern" $ L.indentBlock scn $ do
--                   key <- pKey
--                   pColon
--                   return (L.IndentSome Nothing (pure . singleton key . Block) pMapping)
--                 manyPattern = dbg "manyPattern" $ L.indentBlock scn $ do
--                   key <- pKey
--                   pColon
--                   return (L.IndentMany Nothing (pure . singleton key . Block . (\x -> [x]) . Sequence) pOneItem)


top :: Parser [Yaml]
top = many (L.nonIndented scn (dbg "allPattern" p))
-- top = Block . (\x -> [x]) . Sequence <$> many (L.nonIndented scn (dbg "allPattern" p))
  where
    singleton :: Key -> Yaml -> Yaml
    singleton key = (Block . (\x -> [x]) . Mapping key)
    p :: Parser Yaml
    p = try noPattern <|> try somePattern <|>  try manyPattern
        where
          noPattern = do
            key <- pKey
            pColon
            dbg "noPattern" $ singleton key <$> lexeme pLiteral
          somePattern = dbg "somePattern" $ L.indentBlock scn $ do
            key <- pKey
            pColon
            return (L.IndentSome Nothing (pure . singleton key . Block) pMapping)
          manyPattern = dbg "manyPattern" $ L.indentBlock scn $ do
            key <- pKey
            pColon
            return (L.IndentMany Nothing (pure . singleton key . Block . (\x -> [x]) . Sequence) pOneItem)

pColon :: Parser ()
pColon = lexeme $ void (char ':')

pHyphen :: Parser ()
pHyphen = lexeme $ void (char '-')

pMapping :: Parser Sentence
pMapping = dbg "pMapping" $ do
  key <- pKey
  pColon
  yaml <- pYaml
  return $ Mapping key yaml

pOneItem :: Parser Yaml
pOneItem = dbg "pOneItem" $ pHyphen *> pYaml

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

pMapping' = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- pKey
      pColon
      return (L.IndentMany Nothing (return . (header, )) pMapping)

pMapping'' = L.nonIndented scn (dbg "somePattern" $ L.indentBlock scn p)
  where
    p = do
      root <- pKey
      pColon
      return (L.IndentSome Nothing (pure . Block) pMapping)

pItemList = L.nonIndented scn (L.indentBlock scn p)
  where
    p = do
      header <- pItem
      return (L.IndentMany Nothing (return . (header, )) pItem)

pItem = lexeme (some (alphaNumChar <|> char '-')) <?> "list item"

pp :: Show a => Parser a -> Text -> IO ()
pp parser src = either print pPrint $ parse parser "for pretty print" src

sampleText :: Text
sampleText = T.init $ T.unlines ["name:                play-ground"
                                , "version:             0.1.0."
                                , "github:              \"githubuser/play-ground\""
                                , "license:             BSD"
                                , "author:              \"Author name here\""
                                , "maintainer:          \"example@example.com\""
                                , "copyright:           \"2019 Author name here\""
                                , "extra-source-files:"
                                , "  - README.md"
                                , "  - ChangeLog.md"
                                , "library:"
                                , "  source-dirs: src"
                                -- , "  test:"
                                -- , "    testkey1:"
                                -- , "      testkey2: value"
                                -- , "    testkey3:"
                                -- , "      testkey4: value"
                                ]
