{-# LANGUAGE QuasiQuotes, FlexibleContexts #-}

module Parser (
	parseDef,
	Def(..), Lambda(..),
	testDef
) where

import Text.Peggy
import System.IO.Unsafe

testDef :: [Def]
Right testDef = parseDef testInp

testInp :: String
testInp = unsafePerformIO $ readFile "test.mhs"

data Def = Def { defVar :: String,  defBody :: Lambda } deriving Show
data Lambda
	= Int Int
	| Var String
	| Apply Lambda Lambda
	| Lambda String Lambda
	deriving Show

parseDef :: String -> Either ParseError [Def]
parseDef = parseString defs ""

applyAll :: [Lambda] -> Lambda
applyAll (l : ls) = foldl Apply l ls

[peggy|

defs :: [Def]
	= def '\n'+ defs	{ $1 : $3 }
	/ ''			{ [] }

def :: Def
	= var sp '=' sp lambda	{ Def $1 $4 }

var :: String
	= [a-z]+

int :: Int
	= [0-9]+		{ read $1 }

apply :: [Lambda]
	= lambda1 (' '+ lambda1)*
				{ $1 : map snd $2 }

mkLambda :: Lambda
	= '\\' var sp '->' sp lambda
				{ Lambda $1 $4 }

lambda :: Lambda
	= apply			{ applyAll $1 }

lambda1 :: Lambda
	= int			{ Int $1 }
	/ var			{ Var $1 }
	/ mkLambda
	/ '(' lambda ')'

sp :: ()
	= ' '*			{ () }

|]
