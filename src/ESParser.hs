module ESParser
  ( parseProposition
  , parseESInput
  ) where

import ESTypes
import Data.List
import GenericParsers
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators
import Control.Monad.Combinators.Expr

parseESInput :: Parser ESData
parseESInput = lexeme $ do
  (KnowledgeBase atomicf complexf) <- parseKnowledgeBase
  initialFacts <- parseInitialFacts
  queries <- parseQueries
  return $ ESData (KnowledgeBase  (nub $ atomicf ++ initialFacts) (nub complexf)) queries

parseInitialFacts :: Parser [AtomicProposition]
parseInitialFacts = lexeme $ do
  symbol "="
  facts <- some upperChar
  return $ map (flip AtomicProposition True) facts

parseQueries :: Parser [AtomicFact]
parseQueries = lexeme $ do
  symbol "?"
  some upperChar

parseKnowledgeBase :: Parser KnowledgeBase
parseKnowledgeBase = lexeme $ do
  propositions <- sc' *> (some $ (sc' *> parseProposition) <* (some eol))
  return $ KnowledgeBase (filterAtomic propositions) (filterComplex propositions)

parseProposition :: Parser Proposition
parseProposition = lexeme' $
      PComplex <$> parseComplexProposition
  <|> PAtomic <$> parseAtomicProposition

parseAtomicProposition :: Parser AtomicProposition
parseAtomicProposition = lexeme' $ do
  (FAtomic atomicFact) <- try (parseAtomicFact <* symbol' "=")
  boolVal <- bool'
  return $ AtomicProposition atomicFact boolVal

parseComplexProposition :: Parser ComplexProposition
parseComplexProposition = lexeme' $ do
  (lhs, conseq) <- try $ (,) <$> parseFact <*> parseConsequence
  rhs <- parseConsFact
  return $ ComplexProposition conseq lhs rhs

parseConsequence :: Parser Consequence
parseConsequence = lexeme' $
  ((string "=>") >> return Implication)
  <|> ((string "<=>") >> return IFF)

parseFact :: Parser Fact
parseFact = makeExprParser parseFactTerm propositionOperators

parseConsFact :: Parser ConsFact
parseConsFact = lexeme' $ parseBinaryConsFact <|> (CFAtomic <$> upperChar)
  where parseBinaryConsFact = do
          (f1, op) <- try $ (,) <$> lexeme' upperChar <*> parseBinOp
          f2 <- lexeme' upperChar
          return $ CFBin op f1 f2

parseFactTerm :: Parser Fact
parseFactTerm
  =   FParens <$> (lexeme' $ parens parseFact)
  <|> FBool   <$> (try bool')
  <|> parseAtomicFact

parseAtomicFact :: Parser Fact
parseAtomicFact = lexeme' $ FAtomic <$> upperChar

parseBinOp :: Parser BinOp
parseBinOp = lexeme' $
      (And <$ symbol "+")
  <|> (Or  <$ symbol "|")
  <|> (Xor <$ symbol "^")

propositionOperators :: [[Operator Parser Fact]]
propositionOperators = [
    [ Prefix (FUnary Not  <$ symbol "!") ]
  , [ InfixL (FBinary And <$ symbol "+")
    , InfixL (FBinary Or  <$ symbol "|")
    , InfixL (FBinary Xor <$ symbol "^")
    ]
  ]
