module Environment (Env, env0, refer) where

import qualified Data.Map as M

import Value (Value(..), Symbol, Error(..), ErrorMessage)

unboundErr :: ErrorMessage
unboundErr = "*** ERROR: unbound variable: "

type Env = M.Map Symbol Value

env0 :: Env
env0 = M.fromList [
	("exit", DoExit)
	]

refer :: Symbol -> Env -> Either Error Value
refer s e = maybe (Left . Error $ unboundErr ++ s) Right (M.lookup s e)
