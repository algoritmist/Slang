module Executor where

import Compiler hiding (toMeta, toMetaExprs, varToStr)
import Language
import Parser (program)
import Stack
import Utils

-- this function gets String input and returns the result of program execution
--execute :: String -> String
--execute = toState . program

-- to reduce and expression we should be aware of global State
reduce :: MetaExpr -> State -> (MetaExpr, State)
reduce (VLabel n) st = (maybe ILegal Num (get n $ stack st), st)
-- exprs not fully redused here for laziness!
reduce (FunCall fun exprs) st = execute fun exprs' st'
  where
    (exprs', st') = weakReduce exprs st
reduce (UnOp op expr) st = applyUnOp op $ reduce expr st
reduce (BinOp op expr1 expr2) st = applyBinOp op e1 e2 st''
  where
    (e1, st') = reduce expr1 st
    (e2, st'') = reduce expr2 st'
-- any other expr wont be reduced
reduce expr st = (expr, st) -- let & case not supported yet

weakReduce :: MetaEpxr -> State -> (MetaExpr, State)
-- if its a variable then replace with stack value
weakReduce (VLabel n) st = reduce (VLabel n) st
-- else try reducing subexpr
weakReduce (FunCall fun exprs) st = (FunCall fun exprs', st')
  where
    reduces [] state = ([], state)
    reduces (x : xs) state = (x', state') : reduces xs state'
      where
        (x', state') = weakReduce x state
    (exprs', sts') = reduces exprs st
    st' = last sts'
weakReduce (UnOp op expr) st = (UnOp op expr', st')
  where
    (expr', st') = weakReduce expr st
weakReduce (BinOp op expr1 expr2) st = (BinOp op e1 e2, st'')
  where
    (e1, st') = weakReduce expr1 st
    (e2, st'') = weakReduce expr2 st'
weakReduce expr st = (expr, st)

applyUnOp :: UnaryOperation -> MetaExpr -> State -> (MetaExpr, State)
applyUnOp Neg expr st = case expr of
  (Num x) -> (st, Num $ - x)
  _ -> (st, UnOp Neg expr) -- if cant reduce dont reduce, it'll be useful for laziness

applyBinOp :: BinaryOperation -> MetaExpr -> MetaExpr -> State -> (MetaExpr, State)
applyBinOp Sum (Num x) (Num y) st = (Num $ x + y, st)
applyBinOp Sub (Num x) (Num y) st = (Num $ x - y, st)
applyBinOp Mul (Num x) (Num y) st = (Num $ x * y, st)
applyBinOp Div (Num x) (Num y) st = (Num $ x / y, st)
applyBinOp _ _ _ st = (ILegal, st) --other binops not supported yet for simplicity

type FName = String

-- we need to get variable value from stack
execute :: FName -> [MetaExpr] -> State -> (MetaExpr, State)
execute fun exprs state = do
  len <- length exprs
  addr <- elemIndex' (fun, len) $ functions state -- get the address of expr in stack
  if addr == -1
    then --writeStats state
      return $ error $ "Function \'" ++ fun ++ "\' with " ++ show len ++ "arguments not defined"
    else do
      -- variable stack should be mutable!
      addr' <- get addr $ instructions state
      if addr' == Nothing
        then return $ error $ "Internal error: no expression for function \'" ++ fun ++ "\'"
        else do
          new <- writeStack exprs state
          (expr, st) <- reduce (unwrap addr') new
          return (expr, popStack len st)
