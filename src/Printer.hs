module Printer where

import Language

-- print :: CoreProgram -> String

{-pprExpr :: CoreExpr -> String
pprExpr (ENum n) = show n
pprExpr (EVar v) = v
--pprExpr (EBinOp e1 op e2) = concat [pprExpr e1, " ", op, " ", pprExpr e2]
pprExpr (EAp e1 e2) = concat [pprExpr e1, " ", pprExpr e2]
pprExpr (ELet defs expr) =
  concat
    [ keyword,
      iNewLine,
      " ",
      pprDefs defs,
      iNewLine,
      "in ",
      pprExpr expr
    ]
  where
    keyword = "let"

pprDefs :: [(Name, CoreExpr)] -> String
pprDefs = concatMap sep
  where
    sep x = concat [pprDef x, ";\n"]

pprDef :: (Name, CoreExpr) -> String
pprDef (name, expr) = concat [name, " = ", pprExpr expr]

pprAExpr :: CoreExpr -> String
pprAExpr e
  | isAtomicExpr e = pprExpr e
  | otherwise = concat ["(", pprExpr e, ")"]

iNewLine :: String
iNewLine = "\n"-}
