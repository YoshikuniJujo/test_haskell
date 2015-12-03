{-# LANGUAGE TupleSections #-}

module Eval (eval) where

import Control.Applicative ((<$>))
import Control.Arrow (first, (***))
import Environment (Env, refer, set, Value(..), showValue, Error(..), ErrorMessage)

appErr, prpLstErr, wrongNumberErr :: ErrorMessage
appErr = "*** ERROR: invalid application: "
prpLstErr = "*** ERROR: Compile Error: proper list required: "
wrongNumberErr = "*** ERROR: wrong number of arguments"

eval :: Value -> Env -> Either Error (Value, Env)
eval (Symbol s) e = (, e) <$> refer s e
eval (Cons v vs) e = uncurry (flip apply vs) =<< eval v e
eval v e = Right (v, e)

apply :: Value -> Value -> Env -> Either Error (Value, Env)
apply (Syntax _ s) vs e = s vs e
apply (Subroutine _ s) vs e = uncurry s =<< evaluate vs e
apply (Lambda _ fas bd) as e = do
	(as', e') <- evaluate as e
	e'' <- argument fas as' e'
	(v, _) <- begin bd e''
	return (v, e')
apply DoExit Nil _ = Left Exit
apply f as _ = Left . Error $ appErr ++ showValue (f `Cons` as)

evaluate :: Value -> Env -> Either Error (Value, Env)
evaluate Nil e = Right (Nil, e)
evaluate (v `Cons` vs) e =
	uncurry (<$>) . (first . Cons *** evaluate vs) =<< eval v e
evaluate v _ = Left . Error $ prpLstErr ++ showValue v

argument :: Value -> Value -> Env -> Either Error Env
argument (Symbol s) v e = Right $ set s v e
argument Nil Nil e = Right e
argument (Cons (Symbol s) ss) (Cons v vs) e = argument ss vs $ set s v e
argument _ _ _ = Left $ Error wrongNumberErr

begin :: Value -> Env -> Either Error (Value, Env)
begin (v `Cons` Nil) e = eval v e
begin (v `Cons` vs) e = do
	(_, e') <- eval v e
	begin vs e'
begin _ _ = Left $ Error prpLstErr
