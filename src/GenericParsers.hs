module GenericParsers
  ( Parser
  , sc
  , sc'
  , lexeme
  , lexeme'
  , symbol
  , symbol'
  , parens
  , parens'
  , bool
  , bool'
  , identifier
  ) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

type Parser = Parsec Void String

reservedFail :: String -> Parser String
reservedFail s = fail $ "Keyword " ++ (show s) ++ " is a reserved identifier."

sc' :: Parser ()
sc' = L.space space1' lineCmnt empty
  where
    lineCmnt  = L.skipLineComment "#"
    space1' = do
      some $ (char ' ' <|> char '\t' <|> {-- char '\r' <|> --}char '\v')
      return ()

sc :: Parser ()
sc = L.space space1 lineCmnt empty
  where lineCmnt = L.skipLineComment "#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

lexeme' :: Parser a -> Parser a
lexeme' = L.lexeme sc'

symbol :: String -> Parser String
symbol = L.symbol sc

symbol' :: String -> Parser String
symbol' = L.symbol sc'

parens :: Parser a -> Parser a
parens = between (try $ symbol "(") (symbol ")")

parens' :: Parser a -> Parser a
parens' = between (try $ symbol' "(") (symbol' ")")

bool :: Parser Bool
bool = lexeme $ (try parseTrue) <|> parseFalse
  where parseTrue = ((string' "true") <|> (string "@t")) >> return True
        parseFalse = ((string' "false") <|> (string "@f")) >> return False

bool' :: Parser Bool
bool' = lexeme' $ (try parseTrue) <|> parseFalse
  where parseTrue = ((string' "true") <|> (string "@t")) >> return True
        parseFalse = ((string' "false") <|> (string "@f")) >> return False

reserved :: [String]
reserved = []

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p       = (:) <$> letterChar <*> many alphaNumChar
    check x = if x `elem` reserved then
                reservedFail x
              else
                return x
