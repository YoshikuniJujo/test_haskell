{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PicoScheme where

import Control.Arrow
import Data.Map.Strict (Map, (!))
import Data.Char

import qualified Data.Map.Strict as Map

data Token
	= TString String
	| TInt Int
	| OP | CP
	| TDefine
	| TSymbol String
	deriving Show

data Object
	= String String
	| Int Int
	| Define String Object
	| Symbol String
	deriving Show

type Env = Map String Object

picoScheme :: String -> String
picoScheme src = case fst . (`runAll` Map.empty) . parseAll $ lexer src of
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
lexer ('(' : cs) = OP : lexer cs
lexer (')' : cs) = CP : lexer cs
lexer ('d' : 'e' : 'f' : 'i' : 'n' : 'e' : cs) = TDefine : lexer cs
lexer (c : cs) | isAlpha c = TSymbol (c : sym) : lexer r
	where (sym, r) = span isAlpha cs
lexer (c : _) = error $ "lex error `" ++ [c] ++ "'"

parseAll :: [Token] -> [Object]
parseAll [] = []
parseAll ts = let (o, ts') = parse ts in o : parseAll ts'

parse :: [Token] -> (Object, [Token])
parse [] = error "parse error"
parse (TString s : ts) = (String s, ts)
parse (TInt n : ts) = (Int n, ts)
parse (OP : TDefine : TSymbol sym : ts) = case parse ts of
	(o, CP : ts') -> (Define sym o, ts')
	_ -> error $ "parse error: " ++ show ts
parse (TSymbol sym : ts) = (Symbol sym, ts)

runAll :: [Object] -> Env -> ([Object], Env)
runAll [] e = ([], e)
runAll (o : os) e = (o' :) `first` runAll os e'
	where (o', e') = run o e

run :: Object -> Env -> (Object, Env)
run (String s) e = (String s, e)
run (Int i) e = (Int i, e)
run (Define sym o) e = (Symbol sym, Map.insert sym o e)
run (Symbol sym) e = (e ! sym, e)
