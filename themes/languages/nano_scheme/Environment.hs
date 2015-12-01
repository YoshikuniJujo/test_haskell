module Environment (Env, env0, refer) where

import qualified Data.Map as M

import Value (Value(..), Symbol, Error(..))

type Env = M.Map Symbol Value

env0 :: Env
env0 = M.fromList [
	("exit", DoExit)
	]

refer :: Symbol -> Env -> Either Error Value
refer s e = maybe
	(Left . Error $ "*** ERROR: unbound variable: " ++ s)
	Right (M.lookup s e)
