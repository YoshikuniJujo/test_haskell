module Environment (Env, env0) where

import qualified Data.Map as M

import Value (Value, Symbol)

type Env = M.Map Symbol Value

env0 :: Env
env0 = M.fromList []
