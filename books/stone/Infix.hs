module Infix (
	Infix(..),
	solveInfix
) where

data Infix o f
	= UInfix f [(o, f)]
	| Infix (Infix o f) o (Infix o f)
	| Atom f
	deriving Show

solveInfix :: (o -> Int) -> Infix o f -> Infix o f
solveInfix _ _ = undefined
