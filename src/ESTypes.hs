module ESTypes
  ( ESData(..)
  , KnowledgeBase(..)
  , AtomicProposition(..)
  , ComplexProposition(..)
  , Proposition(..)
  , ConsFact(..)
  , AtomicFact
  , Fact(..)
  , Consequence(..)
  , UnOp(..)
  , BinOp(..)
  , filterAtomic
  , filterComplex
  ) where

import Data.List (intercalate)

data ESData = ESData
  { base    :: KnowledgeBase
  , queries :: [AtomicFact]
  }

data KnowledgeBase = KnowledgeBase
  { _atomic  :: [AtomicProposition]
  , _complex :: [ComplexProposition]
  }

data AtomicProposition = AtomicProposition 
  { apFact :: AtomicFact
  , apVal  :: Bool
  } deriving Eq

data ComplexProposition = ComplexProposition Consequence Fact ConsFact
  deriving Eq

data Proposition
  = PComplex ComplexProposition
  | PAtomic AtomicProposition
  deriving Eq

data ConsFact
  = CFBin BinOp AtomicFact AtomicFact
  | CFAtomic AtomicFact
  deriving Eq

type AtomicFact = Char

data Fact
  = FAtomic AtomicFact
  | FBool Bool
  | FUnary UnOp Fact
  | FBinary BinOp Fact Fact
  | FParens Fact
  deriving Eq

data Consequence
  = Implication
  | IFF
  deriving Eq

data UnOp = Not deriving Eq

data BinOp
  = And
  | Or
  | Xor
  deriving Eq


instance Show ESData where
  show (ESData b q) = "Base = {\n"
                        ++ indent (show b)
                        ++ "}\nQueries = {\n"
                        ++ indent (show q)
                        ++ "}"
    where
      indent = unlines . map ("\t" ++) . lines

instance Show KnowledgeBase where
  show (KnowledgeBase atomic complex) = showAtomic ++ showComplex
    where showAtomic = "Atomic = [\n" ++ intercalate "\n" (map (indent . show) atomic) ++ "\n]\n"
          showComplex = "Complex = [\n" ++ intercalate "\n" (map (indent . show) complex) ++ "\n]\n"
          indent = (++) "\t"

instance Show AtomicProposition where
  show (AtomicProposition fact val) = fact :  " = " ++ show val

instance Show ComplexProposition where
  show (ComplexProposition cons f1 f2) = show f1 ++ " " ++ show cons ++ " " ++ show f2

instance Show Proposition where
  show (PComplex complex) = show complex
  show (PAtomic atomic) = show atomic

instance Show Fact where
  show (FAtomic a)  = a : ""
  show (FBool a)    = show a
  show (FUnary a b) = (show a) ++ (show b)
  show (FBinary a b c) = (show b) ++ " " ++ (show a) ++ " " ++ (show c)
  show (FParens a) = "(" ++ (show a) ++ ")"

instance Show ConsFact where
  show (CFBin op f1 f2) = f1 : " " ++ (show op) ++ " " ++ [f2]
  show (CFAtomic f1) = [f1]

instance Show UnOp where
  show Not = "!"

instance Show BinOp where
  show And = "+"
  show Or  = "|"
  show Xor = "^"

instance Show Consequence where
  show Implication = "=>"
  show IFF = "<=>"

isAtomic :: Proposition -> Bool
isAtomic (PAtomic _) = True
isAtomic  _          = False

isComplex :: Proposition -> Bool
isComplex = not . isAtomic

filterAtomic :: [Proposition] -> [AtomicProposition]
filterAtomic = map (\(PAtomic a) -> a) . filter isAtomic

filterComplex :: [Proposition] -> [ComplexProposition]
filterComplex = map (\(PComplex a) -> a) . filter isComplex
