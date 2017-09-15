{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

infix 4 :==:
infixl 6 :+:
infixl 7 :*:

data Expr a where
	I :: Int -> Expr Int
	B :: Bool -> Expr Bool
	(:+:) :: Expr Int -> Expr Int -> Expr Int
	(:*:) :: Expr Int -> Expr Int -> Expr Int
	(:==:) :: Eq a => Expr a -> Expr a -> Expr Bool

instance Show (Expr a) where
	show (I n) = "(I " ++ show n ++ ")"
	show (B b) = "(B " ++ show b ++ ")"
	show (e1 :+: e2) = "(" ++ show e1 ++ " :+: " ++ show e2 ++ ")"
	show (e1 :*: e2) = "(" ++ show e1 ++ " :*: " ++ show e2 ++ ")"
	show (e1 :==: e2) = "(" ++ show e1 ++ " :==: " ++ show e2 ++ ")"

eval :: Expr a -> a
eval (I n) = n
eval (B b) = b
eval (e1 :+: e2) = eval e1 + eval e2
eval (e1 :*: e2) = eval e1 * eval e2
eval (e1 :==: e2) = eval e1 == eval e2
