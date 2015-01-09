module Infix (
	Infix(..),
	solveInfix,
	mapInfix
) where

import Control.Arrow

data Infix o f
	= UInfix (Infix o f) [(o, Infix o f)]
	| Infix (Infix o f) o (Infix o f)
	| Atom f
	deriving Show

solveInfix :: (o -> Int) -> Infix o f -> Infix o f
solveInfix _ (UInfix l [(o, r)]) = Infix l o r
solveInfix power (UInfix l ((o1, r1) : rest@((o2, r2) : _))) =
	if power o1 >= power o2
		then solveInfix power $ UInfix (Infix l o1 r1) rest
		else Infix l o1 $ solveInfix power $ UInfix r1 rest
solveInfix _ solved = solved

mapInfix :: (f -> g) -> Infix o f -> Infix o g
mapInfix f (Atom x) = Atom $ f x
mapInfix f (Infix i o j) = Infix (mapInfix f i) o (mapInfix f j)
mapInfix f (UInfix i ofs) =
	UInfix (mapInfix f i) (map (second $ mapInfix f) ofs)
