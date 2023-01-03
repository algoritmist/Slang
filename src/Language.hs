module Language where

import Utils

data Expr a
  = EVar Name -- Variables
  | ENum Int -- Numbers
  | EConstr Int Int -- Constructors
  | EAp (Expr a) (Expr a) -- Applications
  | ELet -- let(rec) expressions,
      IsRec -- True if recursive,
      [(a, Expr a)] -- list of bound variables,
      (Expr a) -- returned expression
  | ECase -- case expression
      (Expr a) -- expression to scrutinise
      [Alter a] -- alternatives
  | ELam [a] (Expr a) -- lambda abstraction
  deriving (Show)

type Name = String

type Alter a = (Int, [a], Expr a) -- (tag, bound variables list, expression)

type CoreAlt = Alter Name

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _ = False

type Program a = [ScDef a]

type CoreProgram = Program Name

type ScDef a = (Name, [a], Expr a)

type CoreScDef = ScDef Name
