module OpTree where
import Language
data OpTree = OpFunctionDefine Label Args | OpStore Data | OpLoad Data 
    | UnOp UnaryOperation OpTree | BinOp BinaryOperation OpTree OpTree | OpStub CoreExpr deriving (Show)

{--toOp :: Expr a -> OpTree
toOp (EInt x) = OpStore x
toOp (EFloat x) = OpStore x
toOp (EList x) = OpStore x
toOp (EString x) = OpStore x
toOP _ = error "Check code for non-primitive conversion"
--}
type Data = CoreExpr
type Label = String
type Args = [CoreExpr]

type LinearCode = OpTree
translate :: OpTree -> LinearCode
translate = id