module Compiler where

import Data.List
import Language
import Parser
import Data.Tuple.Select
import Stack
-- runProg :: String -> String

type Addr = Int

type FunctionStack = Stack (String, Addr) -- number of args & where to jump

type InstructionStack = Stack MetaExpr -- Modified CoreExpr

data Heap = Heap deriving (Show)

data Stats = Stats deriving (Show)

type MetaFunction = (String, Int)

type ArgStack = Stack Int

data State = State {functions :: FunctionStack, instructions :: InstructionStack, stack :: ArgStack, heap :: Heap, stats :: Stats} deriving (Show)


toExecFunction :: CoreDefinition -> MetaFunction
toExecFunction (f, bounds, _) = (f, length bounds)

functionList :: CoreProgram -> [MetaFunction]
functionList = map toExecFunction

data MetaExpr
  = VLabel Int -- Variable label
  | Num Int -- Numbers
  | Constr Int Int -- Constructors
  | UnOp UnaryOperation MetaExpr
  | BinOp BinaryOperation MetaExpr MetaExpr -- binary operation
  | FunCall Int [MetaExpr] -- Applications ?
  {-| ELet -- let(rec) expressions,
  --IsRec -- True if recursive,
      CoreVarDefinition -- list of bound variables,
      MetaExpr -- returned expression
  | ECase -- case expression
      MetaExpr -- expression to scrutinise
      [MetaAlter] -- alternatives
  | ELam [String] MetaExpr -- lambda abstraction-}
  deriving (Show)

type MetaProgram = [MetaExpr]

toMeta :: [String] -> [MetaFunction] -> CoreExpr -> MetaExpr
toMeta vars _ (EVar x) = case elemIndex x vars of
  (Just n) -> VLabel n
  Nothing -> error $ "cant evaluate expression: variable \'" ++ x ++ "\' is free"

toMeta vars fs (EFunCall f exprs) = case elemIndex f (fstMap fs) of
  (Just n) -> FunCall n $ map (toMeta vars fs) exprs
  Nothing -> error $ "cant evaluate expression: function \'" ++ f ++ "\' not defined"

toMeta vars fs (EUnOp op expr) = UnOp op $ toMeta vars fs expr
toMeta vars fs (EBinOp op e1 e2) = BinOp op (toMeta vars fs e1) (toMeta vars fs e2)

toMeta _ _ (ENum x) = Num x

toMeta _ _ _ = error "cant evaluate expression: not supported yet"

fstMap :: [(a, b)] -> [a]
fstMap = fmap fst

varToStr :: CoreExpr -> String
varToStr (EVar x) = x
varToStr _ = error "non-variable bound! check parser"

varsToStr :: [CoreExpr] -> [String]
varsToStr = map varToStr


toMetaExprs :: CoreProgram -> [MetaExpr]
toMetaExprs xs = map (\(vars, expr) -> toMeta vars functionStack expr) sndPart where
  -- step 1: function stack is assembled of functions and number of arguments
  -- now we need to replace each function with its address on the stack
  functionStack = functionList xs
  -- step 2: we also need to replace variable with its label, to get values from stack
  sndPart = map (\x -> (varsToStr $ sel2 x, sel3 x)) xs


toState :: CoreProgram -> State
toState xs = State functions instructions Heap Stats where
  functions = toStack $ functionList xs
  instructions = toStack $ toMetaExprs xs