module Translator where
import qualified Data.Map as Map
import qualified GFC      as GFC
import qualified ISA      as ISA
import qualified Language as Language

type Name = String
type Variable = String
data RegisterType = Argument | Saved | Temporary | StackPointer Int | Stack Int deriving(Show)
data Register = Register RegisterType Name deriving(Show)


-- I used this name instead of MemoryMapper to avoid confutsion. It's a R-R architechure, so data memory is mapped to virtual registers.
data RegisterMapper = RegisterMapper{registers :: [Register], registerMap :: Map Variable Register}

allocateNamedRegister :: RegisterMapper -> Name -> RegisterType -> RegisterMapper
allocateAnonymousRegister :: RegisterMapper -> RegisterType -> (RegisterMapper, Name)
deallocateRegister :: RegisterMapper -> Name -> RegisterMapper
getRegister :: RegisterMapper -> Name -> Maybe Register

{-
I use a simple algorithm for register allocation. It tracks registers associated with results of expressions using a Map.  When calling a function all A and S registers are saved to the stack and restored after exiting the call.
One could do a better job tracking the usage of A registers and saving only the necessary ones.
If there is no free "real" registers we will put values on the stack wich is mapped to "virtual" registers.
-}

translate :: CoreExpr -> [Instruction]
tanslate expr = fst $ translateHelper expr $ RegisterMapper registers Map.empty

translateHelper :: CoreExpr -> RegisterMapper -> ([Instruction], RegisterMapper, Register)

translateHelper (Language.EDefinition function) mp =
	let
		label = P $ ISA.Label $ name function
		mp' = mapRegisters mp $ args function
		(isa, mp'', r) = translateHelper (expression function) mp'
	in (isa ++ [RR $ ISA.Add a0 r zero, F $ ISA.Ret], mp, a0)

translateHelper (Language.EVarDefinition name expr) mp =
	let
		(isa, mp', r) = translateHelper expr mp
		mp'' = associateeRegister mp r name
	in (isa, mp'', r)

translateHelper (Language.EFunCall name splitExpr) mp = 
	let
		args = toArgs splitExpr
		(isa, mp') = putArgs mp
		-- before calling we need to save the return address, but this will be done with Loader
	in (isa ++ [P $ ISA.LabelCall name], mp', a0)

translateHelper (Language.EUnOp op expr) mp =
	let
		(mp', rd) = allocAnonymousRegister mp Temporary
		(isa, mp'', r) = translate expr mp'
		instructions = case op of
									 Language.Neg -> [RR $ ISA.Sub rd zero r]
									 Language.Not -> [RR $ ISA.Not rd r]
	in (isa ++ instructions, mp', rd)

{- Boolean binary operators in if statements are actualy triple operators, because ISA allows us to jump to an address immediatly after comparison.
	For simplisity I'll use pseudo operations to compare them and do real jumps from the flow-control graph
-}
translateHelper (Language.EBinOp op e1 e2) mp =
	let
		(mp', rd) = allocAnonymousRegister mp Temporary
		(isa1, mp'', r1) = translate e1 mp'
		(isa2, mp''', r2) = translate e2 mp''
		instructions = case op of
									Language.Sum -> [RR $ ISA.Add rd r1 r2]
									Language.Sub -> [RR $ ISA.Sub rd r1 r2]
									Language.Mul -> [RR $ ISA.Mul rd r1 r2]
									Language.Div -> [RR $ ISA.Div rd r1 r2]
									Language.And -> [RR $ ISA.And rd r1 r2]
									Language.Or  -> [RR $ ISA.Or rd r1 r2]
									Language.Eq  ->	[RR $ ISA.Eq rd r1 r2]
									otherwise -> error "Other operations not supported yet"
	in (isa1 ++ isa2 ++ instructions, mp', rd)



-- Constants should be placed in temporary registes
translateHelper (Lanugage.EInt x) mp =
	let
		mp' = allocRegister mp "const" Temporary
		reg = getRegister mp' "const"
		result = case reg of 
						(Just r) -> ([RR $ ISA.AddI (getName reg) zero x], mp', r)
						Nothing -> error "Failed to allocate register for constant value " ++ show x
	in
		result

-- Translating variable by itself means getting the register mapped to it. If there is no such register, the variable is not defined and an error will be shown.
translateHelper (Lanugage.EVar x) mp =
	let
		reg = getRegister mp x
		result = case reg of
						 (Just r) -> ([], mp', r)
						 Nothing -> error "Variable " ++ show x ++ " not defined"
	in result

translateGFC :: GFC -> RegisterMapper -> ([ISA], RegisterMapper, Register)
translateGFC Tip mp = ([], mp, zero)
translateGFC (Leaf expr) mp = 
	let
		(isa, mp', r) = translateHelper expr mp
	in (isa ++ [F $ Ret], mp', r)
translateGFC (Node x Tip expr) = 
	let
		(isa1, mp',  r1) = translateHelper expr mp
		(isa2, mp'', r2) = translateGFC x mp'
	in (isa1 ++ isa2, mp'', r2)

-- When transating flow-control statements we have to place the results of different branches to a common register. By convention it's t0
translateGFC (Node trueNode falseNode expr) mp = 
	let
		(conditionIsa, mpCondition, r1) = translateHelper expr mp
		(trueIsa, mpTrue, r2) = translateGFC trueNode conditionIsa
 		(falseIsa, mpFalse, r3) = translateGFC falseNode conditionIsa
		trueLabel = Label ???
		falseLabel = Label ??? 
	return (conditionIsa ++ [JNE r1 zero trueLabel] ++ (falseIsa ++ [Mov r2 t0]) ++ (trueIsa ++ [Mov r1 t0]), t0)
