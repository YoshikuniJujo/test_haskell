module Parse (Token, tokens, parse) where

import Control.Applicative
import Data.Char

import Value

data Token
	= TSymbol Symbol
	| TInteger Integer
	| OParen | CParen
	deriving Show

tokens :: String -> Either Error [Token]
tokens ('(' : cs) = (OParen :) <$> tokens cs
tokens (')' : cs) = (CParen :) <$> tokens cs
tokens s@(c : _)
	| isDigit c = do
		let (ds, s') = span isDigit s
		ts <- tokens s'
		return $ TInteger (read ds) : ts
	| otherwise = do
		let (ss, s') = span isAlpha s
		ts <- tokens s'
		return $ TSymbol ss : ts
tokens _ = Right []

parse :: [Token] -> Either Error (Value, [Token])
parse (TSymbol "exit" : ts) = Right (DoExit, ts)
parse (TInteger i : ts) = Right (Integer i, ts)
parse (OParen : ts) = parseList ts
parse _ = Left $ Error "parse: yet"

parseList :: [Token] -> Either Error (Value, [Token])
parseList (CParen : ts) = Right (Nil, ts)
parseList ts = do
	(v, ts') <- parse ts
	(vs, ts'') <- parseList ts'
	return $ (v `Cons` vs, ts'')
