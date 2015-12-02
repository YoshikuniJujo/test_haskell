{-# LANGUAGE TupleSections #-}

module Eval (eval) where

import Control.Applicative ((<$>))
import Control.Arrow (first, (***))
import Environment (Env, refer, Value(..), showValue, Error(..), ErrorMessage)

appErr, evaluateErr :: ErrorMessage
appErr = "*** ERROR: invalid application: "
evaluateErr = "*** ERROR: Compile Error: proper list required: "

eval :: Value -> Env -> Either Error (Value, Env)
eval (Symbol s) e = (, e) <$> refer s e
eval (Cons v vs) e = uncurry (flip apply vs) =<< eval v e
eval v e = Right (v, e)

apply :: Value -> Value -> Env -> Either Error (Value, Env)
apply (Syntax _ s) vs e = s vs e
apply (Subroutine _ s) vs e = uncurry s =<< evaluate vs e
apply (Lambda _ fas bd) as e = undefined
apply DoExit Nil _ = Left Exit
apply f as _ = Left . Error $ appErr ++ showValue (f `Cons` as)

evaluate :: Value -> Env -> Either Error (Value, Env)
evaluate Nil e = Right (Nil, e)
evaluate (v `Cons` vs) e =
	uncurry (<$>) . (first . Cons *** evaluate vs) =<< eval v e
evaluate v _ = Left . Error $ evaluateErr ++ showValue v

argument :: Value -> Value -> Either Error Value
argument _ _ = undefined

begin :: Value -> Env -> Either Error (Value, Env)
begin _ _ = undefined
