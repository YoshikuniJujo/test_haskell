module Parse (Token, tokens, parse) where

import Control.Applicative ((<$>))
import Data.Char (isDigit, isAlpha, isSpace)

import Value (Value(..), Symbol, Error(..), ErrorMessage)

data Token
	= TkSymbol Symbol
	| TkInteger Integer
	| OParen | CParen
	deriving Show

syntaxErr, tokenErr, parseErr :: ErrorMessage
syntaxErr = "*** SYNTAX-ERROR: "
tokenErr = "Can't tokenize: "
parseErr = "Parse error: "

tokens :: String -> Either Error [Token]
tokens ('(' : s) = (OParen :) <$> tokens s
tokens (')' : s) = (CParen :) <$> tokens s
tokens str@(c : s)
	| isAlpha c = do
		let (sm, s') = span isAlpha s
		ts <- tokens s'
		return $ TkSymbol (c : sm) : ts
	| isDigit c = do
		let (ds, s') = span isDigit s
		ts <- tokens s'
		return $ TkInteger (read $ c : ds) : ts
	| isSpace c = tokens s
	| otherwise = Left . Error $ syntaxErr ++ tokenErr ++ show str
tokens _ = return []

parse :: [Token] -> Either Error [Value]
parse [] = return []
parse ts = do
	(v, ts') <- parse1 ts
	(v :) <$> parse ts'

parse1, parseList :: [Token] -> Either Error (Value, [Token])
parse1 (TkSymbol s : ts) = return (Symbol s, ts)
parse1 (TkInteger i : ts) = return (Integer i, ts)
parse1 (OParen : ts) = parseList ts
parse1 ts = Left . Error $ syntaxErr ++ parseErr ++ show ts

parseList (CParen : ts) = return (Nil, ts)
parseList ts = do
	(v, ts') <- parse1 ts
	(vs, ts'') <- parseList ts'
	return $ (v `Cons` vs, ts'')
