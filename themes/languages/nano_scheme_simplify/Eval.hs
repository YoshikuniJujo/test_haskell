{-# LANGUAGE TupleSections #-}

module Eval (eval) where

import Control.Applicative ((<$>))
import Control.Arrow (first, second, (***))
import Environment (
	Env, refer, set, Value(..), showValue, ErrMsg,
	appErr, prpLstErr, wrongNumberErr)

eval :: Value -> Env -> Either ErrMsg (Value, Env)
eval (Symbol s) e = (, e) <$> refer s e
eval (Cons v vs) e = uncurry (flip apply vs) =<< eval v e
eval v e = Right (v, e)

apply :: Value -> Value -> Env -> Either ErrMsg (Value, Env)
apply (Syntax _ s) vs e = s vs e
apply (Subroutine _ s) vs e = uncurry s =<< evals vs e
apply (Lambda _ ps bd) as e = evals as e >>= \(as', e') ->
	second (const e') <$> (begin bd =<< argument ps as' e')
apply f as _ = Left $ appErr ++ showValue (f `Cons` as)

evals :: Value -> Env -> Either ErrMsg (Value, Env)
evals Nil e = Right (Nil, e)
evals (v `Cons` vs) e = uncurry (<$>) . (first . Cons *** evals vs) =<< eval v e
evals v _ = Left $ prpLstErr ++ showValue v

argument :: Value -> Value -> Env -> Either ErrMsg Env
argument (Cons (Symbol p) ps) (Cons a as) e = set p a <$> argument ps as e
argument Nil Nil e = Right e
argument _ _ _ = Left wrongNumberErr

begin :: Value -> Env -> Either ErrMsg (Value, Env)
begin (v `Cons` Nil) e = eval v e
begin (v `Cons` vs) e = begin vs . snd =<< eval v e
begin _ _ = Left prpLstErr
