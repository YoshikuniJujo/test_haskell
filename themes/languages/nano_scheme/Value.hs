module Value (Value(..), Symbol, showValue, Error(..), ErrorMessage) where

data Value
	= Symbol Symbol
	| Integer Integer
	| Cons Value Value | Nil
	| DoExit
	deriving Show

type Symbol = String

showValue :: Value -> String
showValue (Symbol s) = s
showValue (Integer i) = show i
showValue (Cons v vs) = '(' : showCons v vs ++ ")"
showValue Nil = "()"
showValue DoExit = "#<closure exit>"

showCons :: Value -> Value -> String
showCons v Nil = showValue v
showCons v (Cons v' vs) = showValue v ++ " " ++ showCons v' vs
showCons v1 v2 = showValue v1 ++ " . " ++ showValue v2

data Error = Exit | Error ErrorMessage deriving Show

type ErrorMessage = String
