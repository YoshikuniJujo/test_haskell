{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

infix 4 :==:
infixl 6 :+:
infixl 7 :*:

data Expr
	= I Int
	| B Bool
	| Expr :+: Expr
	| Expr :*: Expr
	| Expr :==: Expr
	deriving Show

formula :: Expr -> String
formula (I n) = show n
formula (B b) = show b
formula (e1 :+: e2) = "(" ++ formula e1 ++ " + " ++ formula e2 ++ ")"
formula (e1 :*: e2) = "(" ++ formula e1 ++ " * " ++ formula e2 ++ ")"
formula (e1 :==: e2) = "(" ++ formula e1 ++ " == " ++ formula e2 ++ ")"

eval :: Expr -> Either Int Bool
eval (I n) = Left n
eval (B b) = Right b
{-
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2
-}
