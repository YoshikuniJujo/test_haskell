module Lexer (lexer, Token(..)) where

import Data.Char

data Token
	= OP | CP
	| Define | Lambda
	| Con String | Var String
	| Nat Integer | Str String
	deriving Show

lexer :: String -> [Token]
lexer ('(' : cs) = OP : lexer cs
lexer (')' : cs) = CP : lexer cs
lexer ('"' : cs) = let (s, '"' : r) = span (/= '"') cs in
	Str s : lexer r
lexer ('d' : 'e' : 'f' : 'i' : 'n' : 'e' : cs@(c : _))
	| not $ isAlphaNum c = Define : lexer cs
lexer ('l' : 'a' : 'm' : 'b' : 'd' : 'a' : cs@(c : _))
	| not $ isAlphaNum c = Lambda : lexer cs
lexer s@(c : cs)
	| isSpace c = lexer $ dropWhile isSpace cs
	| isUpper c = let (v, r) = span isAlphaNum s in
		Con v : lexer r
	| isAlpha c = let (v, r) = span isAlphaNum s in
		Var v : lexer r
	| ':' <- c = let (v, r) = span isOperator s in
		Con v : lexer r
	| isOperator c = let (v, r) = span isOperator s in
		Var v : lexer r
	| isDigit c = let (n, r) = span isDigit s in
		Nat (read n) : lexer r
lexer "" = []
lexer s = error $ "lexer: lexical error: " ++ show s

isOperator :: Char -> Bool
isOperator = (`elem` "!#%&*-./:?@$+<=>^|~")
