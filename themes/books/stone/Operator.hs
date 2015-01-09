module Operator (
	operator,
	Inf(..)
) where

import Data.Char

operator :: (a -> (Int, Inf)) -> (a -> a -> a -> a) -> [a] -> a
operator pwr rdc = operator_ (getComp pwr) rdc []

operator_ :: (a -> a -> Bool) -> (a -> a -> a -> a) -> [a] -> [a] -> a
operator_ _ _ [r] [] = r
operator_ cmp rdc (r : o : l : rest) [] = operator_ cmp rdc (rdc l o r : rest) []
operator_ cmp rdc [] (x : xs) = operator_ cmp rdc [x] xs
operator_ cmp rdc [n] (x : x' : xs) = operator_ cmp rdc [x', x, n] xs
operator_ cmp rdc s@(r : o : l : rest) xa@(x : x' : xs)
	| cmp o x = operator_ cmp rdc (rdc l o r : rest) xa
	| otherwise = operator_ cmp rdc (x' : x : s) xs

testOp :: String -> Int
testOp src = case operator getPower reduce $ map ctt src of
	Num n -> n
	_ -> error "bad"

data Token = Num Int | Op Char deriving Show
ctt :: Char -> Token
ctt c	| isDigit c = Num $ read [c]
	| otherwise = Op c

reduce :: Token -> Token -> Token -> Token
reduce (Num l) (Op o) (Num r) = Num $ getOp o l r

getOp :: Char -> Int -> Int -> Int
getOp '+' = (+)
getOp '-' = (-)
getOp '*' = (*)
getOp '/' = div
getOp '%' = mod
getOp '^' = (^)

comp :: Token -> Token -> Bool
comp = getComp getPower

getComp :: (a -> (Int, Inf)) -> a -> a -> Bool
getComp pwr x y = case (pwr x, pwr y) of
	((px, _), (py, _))
		| px > py -> True
		| px < py -> False
	((_, L), (_, L)) -> True
	((_, R), (_, R)) -> False
	_ -> error "bad infix"

getPower :: Token -> (Int, Inf)
getPower (Op c) = (power c, inf c)

power :: Char -> Int
power '+' = 3
power '-' = 3
power '*' = 4
power '/' = 4
power '%' = 4
power '^' = 5

compInf :: Char -> Char -> Bool
compInf o1 o2 = case (inf o1, inf o2) of
	(L, L) -> True
	(R, R) -> False
	_ -> error "bad infixity"

data Inf = L | R deriving Show

inf :: Char -> Inf
inf '+' = L
inf '-' = L
inf '*' = L
inf '/' = L
inf '%' = L
inf '^' = R
