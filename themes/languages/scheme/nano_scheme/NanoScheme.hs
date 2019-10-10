module NanoScheme (scheme, Value, showValue, Env, env0, Error(..)) where

import Control.Applicative ((<$>))
import Control.Arrow (first, (***))

import Primitive (env0)
import Parse (parse, tokens)
import Eval (eval)
import Environment (Env, Value, showValue, Error(..))

scheme :: String -> Env -> Either Error ([Value], Env)
scheme src e = (`evaluate` e) =<< parse =<< tokens src

evaluate :: [Value] -> Env -> Either Error ([Value], Env)
evaluate [] e = Right ([], e)
evaluate (v : vs) e = uncurry (<$>) . (first . (:) *** evaluate vs) =<< eval v e
