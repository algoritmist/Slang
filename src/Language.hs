module Language where

import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Utils

data Expr
  = EVar String -- Variables
  | EInt Int
  | EFloat Double
  | EString String
  | EUnOp UnaryOperation Expr
  | EBinOp BinaryOperation Expr Expr -- binary operation
  | EFunCall String [Expr] -- Applications
  | ELet -- let(rec) expressions,
  --IsRec -- True if recursive,
      [Expr] -- list of bound variables,
      (Expr) -- returned expression
  | ECase -- case expression
      (Expr) -- expression to scrutinise
      [Alter] -- alternatives
  | EOtherwise
  | EIf (Expr) (Alter) -- if statement, otherwise == else
  | EList [Expr] -- should we check list contains elements of one type?
  | EBool Bool
  | EDefinition (Function)
  deriving (Show)

type CoreExpr = Expr

type Alter = (Expr, Expr) -- (tag, bound variables list?, expression)

type CoreAlter = Alter

isAtomicExpr :: Expr -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (EInt _) = True
isAtomicExpr (EFloat _) = True
isAtomicExpr (EString _) = True
isAtomicExpr _ = False



type Program = [Expr]
type CoreProgram = Program
type CoreDefinition = Expr

data Function = Function {name :: String, args :: [Expr], expr :: Expr} deriving (Show)
type CoreFunction = Function 
type CoreFunctionName = String

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
      Token.reservedNames = ["let", "letrec", "in", "where", "case", "of", "lambda", "option", "->", "if", "then", "else"],
      Token.reservedOpNames = Map.keys binaryOperations ++ Map.keys unaryOperations
    }
