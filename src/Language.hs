module Language where

import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Utils

data Expr a
  = EVar a -- Variables
  | EInt Int
  | EFloat Double
  | EString String
  | EConstr Int Int -- Constructors
  | EUnOp UnaryOperation (Expr a)
  | EBinOp BinaryOperation (Expr a) (Expr a) -- binary operation
  | EFunCall (Function a) [Expr a] -- Applications
  | ELet -- let(rec) expressions,
  --IsRec -- True if recursive,
      CoreVarDefinition -- list of bound variables,
      (Expr a) -- returned expression
  | ECase -- case expression
      (Expr a) -- expression to scrutinise
      [Alter a] -- alternatives
  | ELam [a] (Expr a) -- lambda abstraction
  | EOtherwise
  deriving (Show)

type CoreExpr = Expr Name

type Name = String

type Alter a = (Expr a, Expr a) -- (tag, bound variables list?, expression)

type CoreAlter = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (EInt _) = True
isAtomicExpr (EFloat _) = True
isAtomicExpr (EString _) = True
isAtomicExpr _ = False

type Program a = [ScDef a]

type CoreProgram = Program Name

type ScDef a = (Function a, [Expr a], Expr a)
type CoreDefinition = ScDef Name

type Function a = a
type CoreFunction = Function Name

type VarDefinition a = (Expr a, Expr a) -- (variable name & value)

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
  | AddList
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
      ("/", Div),
      (":", AddList)
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
      Token.reservedNames = ["let", "letrec", "in", "where", "case", "of", "lambda", "option", "->", "if"],
      Token.reservedOpNames = Map.keys binaryOperations ++ Map.keys unaryOperations
    }
