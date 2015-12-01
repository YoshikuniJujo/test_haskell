module Value (
	Value(..), Symbol, showValue,
	Error(..)) where

data Value
	= Integer Integer
	| Cons Value Value | Nil
	| DoExit
	deriving Show

type Symbol = String

showValue :: Value -> String
showValue (Integer i) = show i
showValue v = show v

data Error = Exit | Error String deriving Show
