module Executor where

import Control.Monad (when)
import Data.Maybe
import Interpreter hiding (toMeta, toMetaExprs, varToStr)
import Language
import Parser (program)
import Stack
import Text.Parsec.Prim (parse)
import Utils

runProgram :: String -> String -> String
runProgram name str = case parse program name str of
  (Left x) -> error $ show x
  (Right prog) -> case run (toState prog) of
    (Num x, _) -> show x
    (Real x, _) -> show x
    (expr, _) -> show expr

run :: State -> (MetaExpr, State)
run state = case getEntyPoint state of
  Nothing -> error "No enty point found"
  Just n -> execute n [] state

reduceAll :: [MetaExpr] -> State -> (MetaExpr -> State -> (MetaExpr, State)) -> ([MetaExpr], State)
reduceAll exprs state reducer = (exprs', st')
  where
    results = reduces exprs state
    st' = snd $ last results
    exprs' = fstMap results

    reduces [] _ = error "cant reduce nothing"
    reduces [x] st = [reducer x st]
    reduces (x : xs) st = (x', state') : reduces xs state'
      where
        (x', state') = reducer x st

-- to reduce and expression we should be aware of global State
reduce :: MetaExpr -> State -> (MetaExpr, State)
reduce (Num x) st = (Num x, st)
reduce (Real x) st = (Real x, st)
reduce (VLabel n) st = reduce expr st
  where
    expr = fromMaybe ILegal (get n $ stack st)
-- exprs not fully reduced here for laziness!
reduce (FunCall fun exprs) st = execute fun exprs' st'
  where
    (exprs', st') = reduceAll exprs st weakReduce
reduce (MCase expr exprs) st = uncurry reduce (lookup' expr exprs st)
reduce (UnOp op expr) st = uncurry (applyUnOp op) (reduce expr st)
reduce (BinOp op expr1 expr2) st = applyBinOp op e1 e2 st''
  where
    (e1, st') = reduce expr1 st
    (e2, st'') = reduce expr2 st'
-- any other expr wont be reduced
reduce _ st = (ILegal, st) -- let & case not supported yet

weakReduce :: MetaExpr -> State -> (MetaExpr, State)
weakReduce (Num x) st = reduce (Num x) st
weakReduce (Real x) st = reduce (Real x) st
-- if its a variable then replace with stack value
weakReduce (VLabel n) st = reduce (VLabel n) st
-- else try reducing subexpr
weakReduce (FunCall fun exprs) st = (FunCall fun exprs', st')
  where
    (exprs', st') = reduceAll exprs st weakReduce
weakReduce (UnOp op expr) st = (UnOp op expr', st')
  where
    (expr', st') = weakReduce expr st
weakReduce (BinOp op expr1 expr2) st = (BinOp op e1 e2, st'')
  where
    (e1, st') = weakReduce expr1 st
    (e2, st'') = weakReduce expr2 st'
weakReduce (MCase ex exs) _ = error "how?"
weakReduce _ st = (ILegal, st)

applyUnOp :: UnaryOperation -> MetaExpr -> State -> (MetaExpr, State)
applyUnOp Neg expr st = case expr of
  (Num x) -> (Num $ - x, st)
  _ -> (UnOp Neg expr, st) -- if cant reduce dont reduce, it'll be useful for laziness

applyBinOp :: BinaryOperation -> MetaExpr -> MetaExpr -> State -> (MetaExpr, State)
applyBinOp Sum (Num x) (Num y) st = (Num $ x + y, st)
applyBinOp Sub (Num x) (Num y) st = (Num $ x - y, st)
applyBinOp Mul (Num x) (Num y) st = (Num $ x * y, st)
applyBinOp Div (Num x) (Num y) st = (Num $ x `div` y, st)
applyBinOp Sum (Real x) (Real y) st = (Real $ x + y, st)
applyBinOp Sub (Real x) (Real y) st = (Real $ x - y, st)
applyBinOp Mul (Real x) (Real y) st = (Real $ x * y, st)
applyBinOp Div (Real x) (Real y) st = (Real $ x / y, st)
applyBinOp _ _ _ st = (ILegal, st) --other binops not supported yet for simplicity

type FNum = Int

-- we need to get variable value from stack

getFunction :: FNum -> State -> Maybe StackFunction
getFunction n state = get n $ functions state

execute :: FNum -> [MetaExpr] -> State -> (MetaExpr, State)
execute fnum exprs state = (expr', new'')
  where
    ((fname, args), ptr) = fromMaybe (("", 0), (-1)) (getFunction fnum state)
    expr = fromMaybe ILegal (get ptr (instructions state))
    new = writeStack exprs state
    (expr', new') = reduce expr new
    new'' = popStack args new'

lookup' :: MetaExpr -> [(MetaExpr, MetaExpr)] -> State -> (MetaExpr, State)
lookup' x xs st = (fromMaybe ILegal (lookup x' xs), st')
  where
    (x', st') = reduce x st
