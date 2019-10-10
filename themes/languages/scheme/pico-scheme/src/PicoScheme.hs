{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PicoScheme where

import Control.Arrow
import Data.Map (Map)
import Data.Char

import qualified Data.Map as Map

data Token
	= TString String
	| TInt Int
	deriving Show

data Object
	= String String
	| Int Int

type Env = Map String Object

picoScheme :: String -> String
picoScheme src = case fst . (`runAll` Map.empty) . parse $ lexer src of
	[] -> error "Nihili est qui nihil amat."
	r -> case last r of
		String s -> s
		Int i -> show i
		_ -> error "Harp not for ever on the same string."

lexer :: String -> [Token]
lexer "" = []
lexer (' ' : cs) = lexer cs
lexer ('\n' : cs) = lexer cs
lexer ('\t' : cs) = lexer cs
lexer ('"' : cs) = case span (/= '"') cs of
	(t, '"' : r) -> TString t : lexer r
	_ -> error "never-ending string"
lexer ca@(c : _) | isDigit c = TInt (read ds) : lexer r
	where (ds, r) = span isDigit ca
lexer _ = error "lex error"

parse :: [Token] -> [Object]
parse [] = []
parse (TString s : ts) = String s : parse ts
parse (TInt n : ts) = Int n : parse ts

runAll :: [Object] -> Env -> ([Object], Env)
runAll [] e = ([], e)
runAll (o : os) e = (o' :) `first` runAll os e'
	where (o', e') = run o e

run :: Object -> Env -> (Object, Env)
run (String s) e = (String s, e)
run (Int i) e = (Int i, e)
