{-# LANGUAGE TupleSections #-}

module Eval (eval) where

import Control.Applicative ((<$>))
import Control.Arrow (first, second, (***))
import Environment (
	Env, refer, set, Value(..), showValue, ErrMsg, appErr, prpErr, argErr)

eval :: Value -> Env -> Either ErrMsg (Value, Env)
eval (Symbol s) e = (, e) <$> refer s e
eval (Cons f as) e = uncurry (flip apply as) =<< eval f e
eval v e = Right (v, e)

apply :: Value -> Value -> Env -> Either ErrMsg (Value, Env)
apply (Syntax _ s) as e = s as e
apply (Subroutine _ s) as e = uncurry s =<< evals as e
apply (Lambda _ ps bd) as e = evals as e >>= \(as', e') ->
	second (const e') <$> (begin bd =<< argument ps as' e')
apply f as _ = Left $ appErr ++ showValue (f `Cons` as)

evals :: Value -> Env -> Either ErrMsg (Value, Env)
evals Nil e = Right (Nil, e)
evals (v `Cons` vs) e = eval v e >>= uncurry (<$>) . (first . Cons *** evals vs)
evals vs _ = Left $ prpErr ++ showValue vs

argument :: Value -> Value -> Env -> Either ErrMsg Env
argument (Cons (Symbol p) ps) (Cons a as) e = set p a <$> argument ps as e
argument Nil Nil e = Right e
argument _ _ _ = Left argErr

begin :: Value -> Env -> Either ErrMsg (Value, Env)
begin (v `Cons` Nil) e = eval v e
begin (v `Cons` vs) e = begin vs . snd =<< eval v e
begin _ _ = Left prpErr
