module GFC where
import           Language
data Node = Tip | Node NextByDefault NextByCondition CoreExpr | Leaf CoreExpr
 deriving (Show)

type NextByDefault = Node
type NextByCondition = Node

getOp :: Node -> Expr
getOp (Node _ _ op) = op
getOp (Leaf op)     = op
getOp Tip           = error "Can't get OpTree from Tip Node"

getLeaf :: Node -> Node
getLeaf Tip            = error "Tip Node has no Leafs"
getLeaf (Node nxt _ _) = getLeaf nxt
getLeaf x              = x

getNextByDefault :: Node -> Node
getNextByDefault (Node x y _) = x

getNextByCondition :: Node -> Node
getNextByCondition (Node x y _) = y

addtoLeafs :: Node -> Node -> Node
addtoLeafs Tip _ = Tip
addtoLeafs x Tip = x -- if no operations will follow don't change anything
addtoLeafs (Leaf x) y = Node y $ Tip x
addtoLeafs x y = Node (addtoLeafs (getNextByDefault x) y) (addtoLeafs (getNextByCondition x) y) $ getOp x

-- TODO: Use monads instead of OpTree constructors?
translate :: CoreExpr -> Node
translate (ELet vars expr) = translateSequenceHelper $ vars ++ [expr]
translate (EDefinition (Function name args value)) =
	let
		evalExpr = translate value
	in
		Node evalExpr Tip (OpFunctionDefine name args)
translate (EIf condition (trueBranch, falseBranch)) = translateIfHelper condition trueBranch falseBranch Tip
translate expr = Leaf $ toOp expr -- all operations not affecting flow control

translateSequenceHelper :: [CoreExpr] -> Node
translateSequenceHelper [] = Tip
translateSequenceHelper (x:xs) = addtoLeafs (translate x) (translateSequenceHelper xs)

translateIfHelper :: CoreExpr -> CoreExpr -> CoreExpr -> Node -> Node
translateIfHelper condition trueBranch falseBranch nextByFlow =
	let
		trueNode = translate trueBranch
		falseNode = translate falseBranch
		conditionNode = Node trueNode falseNode (getOp $ translate condition)
	in
		addtoLeafs conditionNode nextByFlow

genGraphs :: [CoreExpr] -> [Node]
genGraphs = map translate

toArgs :: CoreExpr -> [CoreExpr]
toArgs (EBinOp SplitArgs e1 e2) = e1 : toArgs e2
toArgs expr                     = [expr]
