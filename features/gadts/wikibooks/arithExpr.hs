{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

infixl 6 :+:
infixl 7 :*:

data Expr
	= I Int
	| Expr :+: Expr
	| Expr :*: Expr
	deriving Show

formula :: Expr -> String
formula (I n) = show n
formula (e1 :+: e2) = "(" ++ formula e1 ++ " + " ++ formula e2 ++ ")"
formula (e1 :*: e2) = "(" ++ formula e1 ++ " * " ++ formula e2 ++ ")"

eval :: Expr -> Int
eval (I n) = n
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2
