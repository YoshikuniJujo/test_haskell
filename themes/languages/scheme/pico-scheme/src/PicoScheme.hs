{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PicoScheme where

import Control.Arrow
import Data.Map (Map)

import qualified Data.Map as Map

data Token
	= TString String

data Object
	= String String

type Env = Map String Object

picoScheme :: String -> String
picoScheme src = case fst . (`runAll` Map.empty) . parse $ lexer src of
	[] -> error "Nihili est qui nihil amat."
	r -> case last r of
		String s -> s
		_ -> error "Harp not for ever on the same string."

lexer :: String -> [Token]
lexer "" = []
lexer (' ' : s) = lexer s
lexer ('\n' : s) = lexer s
lexer ('\t' : s) = lexer s
lexer ('"' : s) = case span (/= '"') s of
	(t, '"' : r) -> TString t : lexer r
	_ -> error "never-ending string"
lexer _ = error "lex error"

parse :: [Token] -> [Object]
parse [] = []
parse (TString s : ts) = String s : parse ts

runAll :: [Object] -> Env -> ([Object], Env)
runAll [] e = ([], e)
runAll (o : os) e = (o' :) `first` runAll os e'
	where (o', e') = run o e

run :: Object -> Env -> (Object, Env)
run (String s) e = (String s, e)
