module Language where

import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Utils

data Expr a
  = EVar a -- Variables
  | ENum Int -- Numbers
  | EConstr Int Int -- Constructors
  | EUnOp UnaryOperation (Expr a)
  | EBinOp BinaryOperation (Expr a) (Expr a) -- binary operation
  | EFunCall (Function a) [a] -- Applications
  | ELet -- let(rec) expressions,
  --IsRec -- True if recursive,
      CoreVarDefinition -- list of bound variables,
      (Expr a) -- returned expression
  | ECase -- case expression
      (Expr a) -- expression to scrutinise
      [Alter a] -- alternatives
  | ELam [a] (Expr a) -- lambda abstraction
  deriving (Show)

type CoreExpr = Expr Name

type Name = String

newtype Alter a = Alter (Int, Expr a) deriving (Show) -- (tag, bound variables list?, expression)

type CoreAlter = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _ = False

type Program a = [ScDef a]

type CoreProgram = Program Name

newtype ScDef a = ScDef (Function a, [a], Expr a) deriving Show

type CoreDefinition = ScDef Name

newtype Function a = Function a deriving (Show)
type CoreFunction = Function Name

type VarDefinition a = (a, Expr a)

type CoreVarDefinition = VarDefinition Name

data UnaryOperation = Not | Neg deriving (Show, Eq)

unaryOperations :: Map.Map String UnaryOperation
unaryOperations = Map.fromList [("!", Not), ("-", Neg)]

isUnaryBoolean :: UnaryOperation -> Bool
isUnaryBoolean = (== Not)

data BinaryOperation
  = And
  | Or
  | Eq
  | G
  | L
  | GE
  | LE
  | NotE
  | Sum
  | Sub
  | Mul
  | Div
  deriving (Show, Eq)

binaryOperations :: Map.Map String BinaryOperation
binaryOperations =
  Map.fromList
    [ ("and", And),
      ("or", Or),
      ("==", Eq),
      (">", G),
      ("<", L),
      (">=", GE),
      ("<=", LE),
      ("!=", NotE),
      ("+", Sum),
      ("-", Sub),
      ("*", Mul),
      ("/", Div)
    ]

isBinaryBoolean :: BinaryOperation -> Bool
isBinaryBoolean op = op /= Sum && op /= Sub && op /= Mul && op /= Div

languageDef :: LanguageDef a
languageDef =
  emptyDef
    { Token.commentStart = "{-",
      Token.commentEnd = "-}",
      Token.commentLine = "--",
      Token.identStart = letter,
      Token.identLetter = alphaNum,
      Token.reservedNames = ["let", "letrec", "in", "where", "case", "of", "lambda", "option"],
      Token.reservedOpNames = Map.keys binaryOperations ++ Map.keys unaryOperations
    }
