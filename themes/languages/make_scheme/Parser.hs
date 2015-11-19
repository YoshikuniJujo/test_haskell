module Parser (parse, Value(..), Symbol, Error(..)) where

import Data.Char

data Value
	= Undef | Nil | Bool Bool | Symbol Symbol
	| Integer Integer | String String
	| Cons Value Value
	| DoExit | Display String | Subroutine ([Value] -> Value)
	| Closure [Symbol] Value
	| Define Symbol Value

type Symbol = String

data Error
	= Exit
	| Error String
	deriving Show

parse :: String -> Either Error (Value, String)
parse ('(' : s) = Right $
	(Cons (Symbol $ takeWhile (/= ')') s) Nil, dropWhile (/= ')') s)
parse (c : s)
	| isDigit c = Right $
		(Integer . read $ c : takeWhile isDigit s, dropWhile isDigit s)
parse s = Right $ (Symbol s, "")
