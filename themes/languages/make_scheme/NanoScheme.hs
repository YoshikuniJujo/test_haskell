module NanoScheme (scheme) where

data Value
	= Nil
	| Bool Bool
	| Symbol String
	| Integer Integer
	| String String
	| Cons Value Value

scheme :: String -> String
scheme = id
