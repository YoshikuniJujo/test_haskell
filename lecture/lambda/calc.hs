import Parser

calc :: [Def] -> Lambda
calc ds = case filter ((== "result") . defVar) ds of
	[Def _ l] -> calculate l ds

calculate :: Lambda -> [Def] -> Lambda
calculate (Int i) _ = Int i
calculate (Var v) ds = case filter ((== v) . defVar) ds of
	Def _ l : _ -> calculate l ds
	_ -> error $ "Var " ++ v
calculate (Apply f x) ds = apply f x ds
calculate l _ = l

apply :: Lambda -> Lambda -> [Def] -> Lambda
apply (Var "pred") val ds = case calculate val ds of
	Int i -> Int (i - 1)
apply (Var "succ") val ds = case calculate val ds of
	Int i -> Int (i + 1)
apply (Var "zero") val ds = case calculate val ds of
	Int 0 -> Lambda "_x" (Lambda "_" $ Var "_x")
	Int _ -> Lambda "_" (Lambda "_y" $ Var "_y")
apply (Lambda x body) val ds = calculate (applyVar x val body) ds
				-- calculate body (Def x val : ds)
apply l val ds = apply (calculate l ds) val ds

applyVar :: String -> Lambda -> Lambda -> Lambda
applyVar _ _ (Int i) = Int i
applyVar var val (Var v)
	| var == v = val
	| otherwise = Var v
applyVar var val (Apply f x) = Apply (applyVar var val f) (applyVar var val x)
applyVar var val (Lambda x l) = Lambda x $ applyVar var val l
