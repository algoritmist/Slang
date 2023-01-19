module Language where

import Utils
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Expr a
  = EVar Name -- Variables
  | ENum Int -- Numbers
  | EConstr Int Int -- Constructors
  | EUnOp UnaryOperation (Expr a)
  | EBinOp (Expr a) BinaryOperation (Expr a) -- binary operation
  | EAp (Expr a) (Expr a) -- Applications
  | ELet -- let(rec) expressions,
      --IsRec -- True if recursive,
      [(a, Expr a)] -- list of bound variables,
      (Expr a) -- returned expression
  | ECase -- case expression
      (Expr a) -- expression to scrutinise
      [Alter a] -- alternatives
  | ELam [a] (Expr a) -- lambda abstraction
  deriving (Show)

type CoreExpr = Expr Name

type Name = String

type Alter a = (Int, Expr a) -- (tag, bound variables list?, expression)

type CoreAlter = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _ = False

type Program a = [ScDef a]

type CoreProgram = Program Name

type ScDef a = (Function, [a], Expr a)

type CoreDefinition = ScDef Name

type Function = Name

data UnaryOperation = Not | Neg deriving (Show, Eq)

unaryOperations :: Map.Map String UnaryOperation
unaryOperations = Map.fromList [("!", Not), ("-", Neg)]

isUnaryBoolean :: UnaryOperation -> Bool
isUnaryBoolean = (== Not)

data BinaryOperation = And | Or | Eq | G | L | GE | LE |
  NotE | Sum | Sub | Mul | Div deriving (Show, Eq)


binaryOperations :: Map.Map String BinaryOperation
binaryOperations = Map.fromList [("and", And), ("or", Or),
                                 ("==", Eq), (">", G), ("<", L), (">=", GE), ("<=", LE), ("!=", NotE),
                                 ("+", Sum), ("-", Sub), ("*", Mul), ("/", Div)]
isBinaryBoolean :: BinaryOperation -> Bool
isBinaryBoolean op = op /= Sum && op /= Sub && op /= Mul && op /= Div

languageDef :: LanguageDef a
languageDef =
   emptyDef { Token.commentStart    = "{-"
            , Token.commentEnd      = "-}"
            , Token.commentLine     = "--"
            , Token.identStart      = letter
            , Token.identLetter     = alphaNum
            , Token.reservedNames   = ["let", "letrec", "in", "where", "case", "of", "lambda"]
            , Token.reservedOpNames = Map.keys binaryOperations ++ Map.keys unaryOperations
            }