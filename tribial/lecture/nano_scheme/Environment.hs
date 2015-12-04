module Environment (Env, M.fromList, Value(..), showValue, Symbol) where

import qualified Data.Map as M

type Env = M.Map Symbol Value

data Value =
	Int Integer

showValue :: Value -> String
showValue (Int i) = show i

type Symbol = String
