data Expression
	= Number { value :: Integer }
	| Add Expression Expression
	| Multiply Expression Expression

showExpression :: Expression -> String
showExpression (Number n) = show n
showExpression (Add l r) = showExpression l ++ " + " ++ showExpression r
showExpression (Multiply l r) = showExpression l ++ " * " ++ showExpression r

instance Show Expression where
	show e = "<<" ++ showExpression e ++ ">>"

reduce :: Expression -> Maybe Expression
-- reduce (Add (Number l) (Number r)) = Number $ l + r
-- reduce (Multiply (Number l) (Number r)) =  Number $ l * r
reduce (Add l r)
	| Just l' <- reduce l = Just $ Add l' r
	| Just r' <- reduce r = Just $ Add l r'
	| otherwise = Just . Number $ value l + value r
reduce (Multiply l r)
	| Just l' <- reduce l = Just $ Multiply l' r
	| Just r' <- reduce r = Just $ Multiply l r'
	| otherwise = Just . Number $ value l * value r
reduce _ = Nothing
