{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UntypedLambdaCalculus where

import Control.Arrow
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String

data Variable
	= X | Y | Z | W
	| Primed Variable
	| Numbered Variable Int
	deriving (Eq, Show)

data LambdaTerm
	= Var Variable
	| Lambda Variable LambdaTerm
	| LambdaTerm :$ LambdaTerm
	deriving (Eq, Show)

instance Pretty Variable where
	pretty X = "x"
	pretty Y = "y"
	pretty Z = "z"
	pretty W = "w"
	pretty (Primed v) = pretty v <> "'"
	pretty (Numbered v n) = pretty v <> pretty n

instance Pretty LambdaTerm where
	pretty (Var v) = pretty v
	pretty m@(Lambda _ _) =
		parens $ "λ" <> foldr (<>) "" (pretty <$> vs) <> "." <> pretty m'
		where (vs, m') = multipleLambda m
	pretty (m :$ n) = parens $ pretty m <> pretty n

multipleLambda :: LambdaTerm -> ([Variable], LambdaTerm)
multipleLambda (Lambda v m) = (v :) `first` multipleLambda m
multipleLambda m = ([], m)

lambdaLength :: LambdaTerm -> Int
lambdaLength (Var _) = 1
lambdaLength (Lambda _ m) = 1 + lambdaLength m
lambdaLength (m :$ n) = lambdaLength m + lambdaLength n

showPretty :: Pretty a => a -> String
showPretty = renderString . layoutCompact . pretty

putPretty :: Pretty a => a -> IO ()
putPretty = putStrLn . showPretty

vx, vy, vz, vw :: LambdaTerm
[vx, vy, vz, vw] = Var <$> [X, Y, Z, W]

lambda :: [Variable] -> LambdaTerm -> LambdaTerm
lambda [] m = m
lambda (x : xs) m = Lambda x $ lambda xs m

betaConversion :: LambdaTerm -> LambdaTerm
betaConversion (Lambda v m :$ n) = replace m v n
betaConversion m = m

replace :: LambdaTerm -> Variable -> LambdaTerm -> LambdaTerm
replace m@(Var v') v n
	| v' == v = n
	| otherwise = m
replace m@(Lambda v' m') v n
	| v' == v = m
	| otherwise = Lambda v' $ replace m' v n
replace (m :$ n') v n = replace m v n :$ replace n' v n

infix 4 .=.

(.=.) :: LambdaTerm -> LambdaTerm -> Bool
Lambda v m :$ n .=. m' = replace m v n .=. m'
m .=. m' | m == m' = True
m :$ z .=. n :$ z' = z .=. z' && m .=. n
Lambda v m .=. Lambda v' n = v == v' && m .=. n
Var v .=. Var v' = v == v'
n .=. m = m .=. n

eta :: LambdaTerm -> LambdaTerm 
eta (Lambda v m :$ Var v') | v == v' = m
eta m = m
