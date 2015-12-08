module Environment (
	Env, M.fromList, refer, set, Value(..), showValue, Symbol
) where

import qualified Data.Map as M

type Env = M.Map Symbol Value

refer :: Symbol -> Env -> Maybe Value
refer = M.lookup

set :: Symbol -> Value -> Env -> Env
set = M.insert

data Value
	= Symbol Symbol
	| Int Integer
	| List [Value]
	| Sntx Symbol ([Value] -> Env -> Maybe (Value, Env))
	| Subr Symbol ([Value] -> Env -> Maybe (Value, Env))
	| Lmbd [Symbol] Value

showValue :: Value -> String
showValue (Symbol s) = s
showValue (Int i) = show i
showValue (List vs) = "(" ++ unwords (map showValue vs) ++ ")"
showValue (Sntx n _) = "#<syntax " ++ n ++ ">"
showValue (Subr n _) = "#<subr " ++ n ++ ">"
showValue (Lmbd _ _) = "#<lambda>"

type Symbol = String
