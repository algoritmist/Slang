module Printer where

import Language

-- print :: CoreProgram -> String

pprExpr :: CoreExpr -> String
pprExpr (ENum n) = show n
pprExpr (EVar v) = v
pprExpr (EAp e1 e2) = pprExpr e1 ++ " " ++ pprExpr e2
pprExpr (ELet isRec defs expr) =
  mconcat
    [ iStr keyword,
      iNewLine,
      iStr " ",
      iIndent (pprDefs defs),
      iNewLine,
      iStr "in ",
      pprExpr expr
    ]
  where
    keyword
      | isRec == True = "letrec"
      | otherwise = "let"
pprExpr e = error $ "Unknown type of expression: " ++ show e

pprAExpr :: CoreExpr -> String
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise = "(" ++ pprExpr e ++ ")"

data Iseq = Iseq {x :: String} deriving (Show)

instance Monoid Iseq where
  mempty = iNil
  mappend (Iseq x) (Iseq y) = Iseq $ x ++ y

--TODO : monoid instalce for iseq