module Environment (Env, M.fromList, Value(..), showValue, Symbol) where

import qualified Data.Map as M

type Env = M.Map Symbol Value

data Value
	= Symbol Symbol
	| Int Integer

showValue :: Value -> String
showValue (Symbol s) = s
showValue (Int i) = show i

type Symbol = String
