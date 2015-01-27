module Lexer (lexer, Token(..)) where

import Data.Char

data Token = OP | CP | Var String | Nat Int | Str String
	deriving Show

lexer :: String -> [Token]
lexer ('(' : cs) = OP : lexer cs
lexer (')' : cs) = CP : lexer cs
lexer ('"' : cs) =
	let (s, '"' : r) = span (/= '"') cs in Str s : lexer r
lexer s@(c : cs)
	| isSpace c = lexer $ dropWhile isSpace cs
	| isAlpha c = let (v, r) = span isAlphaNum s in
		Var v : lexer r
	| isDigit c = let (n, r) = span isDigit s in
		Nat (read n) : lexer r
lexer "" = []
lexer _ = error "lexical error"
