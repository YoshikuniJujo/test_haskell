module Eval (eval) where

import Environment
import Value

eval :: Value -> Env -> Either Error (Value, Env)
eval (Cons f as) e = apply f as e
eval v e = Right (v, e)

apply :: Value -> Value -> Env -> Either Error (Value, Env)
apply DoExit Nil _ = Left Exit
apply _ _ _ = Left $ Error "eval: yet"
