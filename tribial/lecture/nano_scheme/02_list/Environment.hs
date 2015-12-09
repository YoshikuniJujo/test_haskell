module Environment (Env, M.fromList, refer, Value(..), showValue, Symbol) where

import qualified Data.Map as M

type Env = M.Map Symbol Value

refer :: Symbol -> Env -> Maybe Value
refer = M.lookup

data Value
	= Sym Symbol
	| Int Integer
	| List [Value]

showValue :: Value -> String
showValue (Sym s) = s
showValue (Int i) = show i
showValue (List vs) = "(" ++ unwords (map showValue vs) ++ ")"

type Symbol = String
