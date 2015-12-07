module Environment (Env, M.fromList, refer, Value(..), showValue, Symbol) where

import qualified Data.Map as M

type Env = M.Map Symbol Value

refer :: Symbol -> Env -> Maybe Value
refer = M.lookup

data Value
	= Symbol Symbol
	| Int Integer
	| List [Value]
	| Subr Symbol ([Value] -> Env -> Maybe (Value, Env))

showValue :: Value -> String
showValue (Symbol s) = s
showValue (Int i) = show i
showValue (List vs) = "(" ++ unwords (map showValue vs) ++ ")"
showValue (Subr n _) = "#<subr " ++ n ++ ">"

type Symbol = String
