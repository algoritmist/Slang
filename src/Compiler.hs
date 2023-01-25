module Compiler where

import Data.List
import Language
import Parser

-- runProg :: String -> String

type Addr = Int

type Stack x = [x]

type FunctionStack = Stack (Int, Int) -- number of args & where to jump

type InstructionStack = Stack ExecExpr -- Modified CoreExpr

data ExecExpr = ExecExpr

data Heap = Heap

data Stats = Stats

data State = State {functions :: FunctionStack, instructions :: InstructionStack, heap :: Heap, stats :: Stats}

type StackFunction = (String, Int)

toExecFunction :: CoreDefinition -> StackFunction
toExecFunction (ScDef (f, bounds, _)) = (f, length bounds)

functionList :: CoreProgram -> [StackFunction]
functionList = map toExecFunction

type FLabel = Int -- first fase of conversion

data MetaExpr
  = VLabel Int -- Variable label
  | Num Int -- Numbers
  | Constr Int Int -- Constructors
  | UnOp UnaryOperation MetaExpr
  | BinOp BinaryOperation MetaExpr MetaExpr -- binary operation
  | FunCall FLabel [MetaExpr] -- Applications ?
  {-| ELet -- let(rec) expressions,
  --IsRec -- True if recursive,
      CoreVarDefinition -- list of bound variables,
      MetaExpr -- returned expression
  | ECase -- case expression
      MetaExpr -- expression to scrutinise
      [MetaAlter] -- alternatives
  | ELam [String] MetaExpr -- lambda abstraction-}
  deriving (Show)

toMeta :: [String] -> [StackFunction] -> CoreExpr -> MetaExpr
toMeta vars _ (EVar x) = case elemIndex x vars of
  (Just n) -> VLabel n
  Nothing -> error $ "cant evaluate expression: variable \'" ++ x ++ "\' is free"

toMeta vars fs (EFunCall f [expr]) = case elemIndex f (fstMap fs) of
  (Just n) -> FunCall n $ map (toMeta vars fs) [expr]
  Nothing -> error $ "cant evaluate expression: function \'" ++ f ++ "\' not defined"

toMeta vars fs (EUnOp op expr) = UnOp op $ toMeta vars fs expr
toMeta vars fs (EBinOp op e1 e2) = BinOp op (toMeta vars fs e1) (toMeta vars fs e2)

toMeta _ _ (ENum x) = Num x

toMeta _ _ _ = error "cant evaluate expression: not supported yet"

compile :: CoreProgram -> State
compile xs = do
  -- step 1: function stack is assembled of functions and number of arguments
  functionStack <- functionList xs
  -- now we need to replace each function with its address on the stack

  return _

fstMap :: [(a, b)] -> [a]
fstMap = fmap fst