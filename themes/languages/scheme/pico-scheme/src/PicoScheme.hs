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
	| TLambda
	| TSymbol String
	deriving Show

data Object
	= String String
	| Int Int
	| Define String Object
	| Symbol String
	| Lambda [String] Object
	| Apply Object [Object]
	deriving Show

type Env = Map String Object

picoScheme :: String -> String
picoScheme src = case fst . (`evalAll` Map.empty) . parseAll $ lexer src of
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
lexer ('l' : 'a' : 'm' : 'b' : 'd' : 'a' : cs) = TLambda : lexer cs
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
parse (OP : TLambda : OP : ts) = case parseSymbols ts of
	(syms, CP : ts') -> case parse ts' of
		(o, CP : ts'') -> (Lambda syms o, ts'')
		_ -> error $ "parse error: " ++ show ts'
	_ -> error $ "parse error: " ++ show ts
parse (OP : ts) = (Apply f args, ts'')
	where
	(f, ts') = parse ts
	(args, ts'') = parseP ts'

parseP :: [Token] -> ([Object], [Token])
parseP (CP : ts) = ([], ts)
parseP ts = let (o, ts') = parse ts in (o :) `first` parseP ts'

parseSymbols :: [Token] -> ([String], [Token])
parseSymbols [] = ([], [])
parseSymbols (TSymbol sym : ts) = (sym :) `first` parseSymbols ts
parseSymbols ts = ([], ts)

evalAll :: [Object] -> Env -> ([Object], Env)
evalAll [] e = ([], e)
evalAll (o : os) e = (o' :) `first` evalAll os e'
	where (o', e') = eval o e

eval :: Object -> Env -> (Object, Env)
eval (String s) e = (String s, e)
eval (Int i) e = (Int i, e)
eval (Define sym o) e = (Symbol sym, Map.insert sym o e)
eval (Symbol sym) e = (e ! sym, e)
eval o@(Lambda _ _) e = (o, e)
eval (Apply f args) e = case eval f e of
	(Lambda syms o, e') -> let
		(args', e'') = evalAll args e'
		e''' = foldr (uncurry Map.insert) e'' (zip syms args')
		(o', _) = eval o e''' in (o', e)
	_ -> error "eval error"
