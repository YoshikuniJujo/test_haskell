{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

-- HALFWAY

module Ch6MakingACentury where

import Data.List

type Expression = [Term]
type Term = [Factor]
type Factor = [Digit]
type Digit = Int

valExpr :: Expression -> Int
valExpr = sum . map valTerm

valTerm :: Term -> Int
valTerm = product . map valFact

valFact :: Factor -> Int
valFact = foldl1 (\n d -> 10 * n + d)

good :: Int -> Bool
good v = v == 100

extend :: Digit -> [Expression] -> [Expression]
extend x [] = [[[[x]]]]
extend x es = concatMap (glue x) es

glue :: Digit -> Expression -> [Expression]
glue _ [] = error "bad input"
glue _ ([] : _) = error "bad input"
glue x ((xs : xss) : xsss) = [
	((x : xs) : xss) : xsss,
	([x] : xs : xss) : xsss,
	[[x]] : (xs : xss) : xsss ]

expressions :: [Digit] -> [Expression]
expressions = foldr extend []

showExpression :: Expression -> String
showExpression = intercalate " + " . (showTerm <$>)

showTerm :: Term -> String
showTerm = intercalate " * " . (showFact <$>)

showFact :: Factor -> String
showFact = concatMap show
