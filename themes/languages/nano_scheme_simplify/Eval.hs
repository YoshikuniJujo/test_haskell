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
apply (Lambda _ ps bd) as e = do
	(as', e') <- evaluate as e
	(v, _) <- begin bd =<< argument ps as' e'
	return (v, e')
apply DoExit Nil _ = Left Exit
apply f as _ = Left . Error $ appErr ++ showValue (f `Cons` as)

evaluate :: Value -> Env -> Either Error (Value, Env)
evaluate Nil e = Right (Nil, e)
evaluate (v `Cons` vs) e =
	uncurry (<$>) . (first . Cons *** evaluate vs) =<< eval v e
evaluate v _ = Left . Error $ prpLstErr ++ showValue v

argument :: Value -> Value -> Env -> Either Error Env
argument (Cons (Symbol p) ps) (Cons a as) e = set p a <$> argument ps as e
argument Nil Nil e = Right e
argument (Symbol p) as e = Right $ set p as e
argument _ _ _ = Left $ Error wrongNumberErr

begin :: Value -> Env -> Either Error (Value, Env)
begin (v `Cons` Nil) e = eval v e
begin (v `Cons` vs) e = begin vs . snd =<< eval v e
begin _ _ = Left $ Error prpLstErr
