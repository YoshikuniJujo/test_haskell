module Value (Value(..), Symbol, showValue, Error(..), ErrorMessage) where

data Value
	= Integer Integer
	| Cons Value Value | Nil
	| DoExit
	deriving Show

type Symbol = String

showValue :: Value -> String
showValue (Integer i) = show i
showValue Nil = "()"
showValue DoExit = "#<closure exit>"
showValue v = show v

data Error = Exit | Error ErrorMessage deriving Show

type ErrorMessage = String
