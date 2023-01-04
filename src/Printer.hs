module Printer where

import Language

-- print :: CoreProgram -> String

pprExpr :: CoreExpr -> Iseq
pprExpr (ENum n) = iStr n
pprExpr (EVar v) = Iseq v
pprExpr (EAp e1 e2) = mconcat [pprExpr e1, Iseq " ", pprExpr e2]
pprExpr (ELet isRec defs expr) =
  mconcat
    [ Iseq keyword,
      iNewLine,
      Iseq " ",
      id (pprDefs defs),
      iNewLine,
      Iseq "in ",
      id (pprExpr expr)
    ]
  where
    keyword
      | isRec = "letrec"
      | otherwise = "let"

pprDefs :: [(Name, CoreExpr)] -> Iseq
pprDefs defs = mconcat $ map sep defs
  where
    sep x = mconcat [pprDef x, Iseq ";\n"]

pprDef :: (Name, CoreExpr) -> Iseq
pprDef (name, expr) = mconcat [iStr name, iStr " = ", id (pprExpr expr)]

pprAExpr :: CoreExpr -> Iseq
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise = mconcat [Iseq "(", pprExpr e, Iseq ")"]

newtype Iseq = Iseq String deriving (Show)

instance Monoid Iseq where
  mempty = Iseq ""

instance Semigroup Iseq where
  (Iseq x) <> (Iseq y) = Iseq $ x ++ y

iStr :: Show a => a -> Iseq
iStr x = Iseq $ show x

iNewLine :: Iseq
iNewLine = Iseq "\n"

iValue :: Iseq -> String
iValue (Iseq x) = x

--TODO : monoid instance for iseq
