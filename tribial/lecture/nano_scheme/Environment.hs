module Environment (Env, M.fromList, refer, Value(..), showValue, Symbol) where

import qualified Data.Map as M

type Env = M.Map Symbol Value

refer :: Symbol -> Env -> Maybe Value
refer = M.lookup

data Value
	= Symbol Symbol
	| Int Integer

showValue :: Value -> String
showValue (Symbol s) = s
showValue (Int i) = show i

type Symbol = String
