
data GFC a
	= GRet -- return from context
 | GVar a
 | GBool Bool
 | GInt Int
 | GDouble Double
 | GString String
 | GList [GFC a]
 | GFunCall (Function a) (OpStoreAll [GFC a])
 | OpStore $ GUnOp UnaryOperation (OpLoad $ GFC a)
 | OpStore $ GBinOp BinaryOPeration (OpLoad $ GFC a) (OpLoad $ GFC a)
 | GIf (GFC a) (Alter a) 
 | GOtherwise -- equivalent of jump emmediate
 deriving (Show)

type CoreGFC = GFC Name
type Name = String

type Alter a = (GFC a, GFC a) -- if(e1) then e2; else e3;

condition :: Alter a -> Expr a
condition = fst

result :: Alter a -> Expr a
result = snd

-- TODO: Where GRet should be used 
translate :: Expr a -> GFC a
translate (Evar a) = GVar a
translate (EBool x) = GBool x
translate (EInt x) = GInt x
translate (EDouble x) = GDouble x
translate (EString x) = GString x
translate (GList xs) = GList xs
translate (EFuncall f args) = GFuncall f (OpStoreAll args)
translate (EUnOp op expr) = OpStore $ GUnOp op (OpLoad $ translate expr)
translate (EbinOp op e1 e2) = OpStore $ GBinOp op (OpLoad $ translate e1) (OpLoad $ translate e2)

translate (ECase expr []) = GError -- throw exception if no match
translate (ECase expr alt:alters) = 
	GIf 
	(
		GEqals 
			(translate expr)
			(translate $ condition alt)
	)
		(translate $ result alt, translate (ECase expr alters))
		-- case is syntaxic shugar, translate it to if-expression
translate EOtherwise = GOtherwise
translate (EIf condition (etrue, efalse)) = 
	GIf
	(
		GEq (translate condition) GTrue
 	)
	(translate etrue)
	(translate efalse)

