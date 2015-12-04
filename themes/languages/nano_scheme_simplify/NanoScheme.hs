module NanoScheme (scheme, Value, showValue, Env, env0, ErrMsg) where

import Control.Applicative ((<$>))
import Control.Arrow (first, (***))

import Primitive (env0)
import Eval (eval)
import Parse (parse, tokens)
import Environment (Env, Value, showValue, ErrMsg)

scheme :: String -> Env -> Either ErrMsg ([Value], Env)
scheme src e = (`evaluate` e) =<< parse =<< tokens src

evaluate :: [Value] -> Env -> Either ErrMsg ([Value], Env)
evaluate [] e = Right ([], e)
evaluate (v : vs) e = uncurry (<$>) . (first . (:) *** evaluate vs) =<< eval v e
