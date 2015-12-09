module Environment (
	Env, M.fromList, refer, set, Value(..), showValue, Symbol) where

import qualified Data.Map as M

type Env = M.Map Symbol Value

refer :: Symbol -> Env -> Maybe Value
refer = M.lookup

set :: Symbol -> Value -> Env -> Env
set = M.insert

data Value
	= Sym Symbol
	| Int Integer
	| List [Value]
	| Sntx ([Value] -> Env -> Maybe (Value, Env))
	| Subr ([Value] -> Env -> Maybe (Value, Env))

showValue :: Value -> String
showValue (Sym s) = s
showValue (Int i) = show i
showValue (Sntx _) = "#<syntax>"
showValue (Subr _) = "#<subr>"
showValue (List vs) = "(" ++ unwords (map showValue vs) ++ ")"

type Symbol = String
