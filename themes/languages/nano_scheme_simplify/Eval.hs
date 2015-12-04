{-# LANGUAGE TupleSections #-}

module Eval (eval) where

import Control.Applicative ((<$>))
import Control.Arrow (first, second, (***))
import Environment (Env, refer, set, Value(..))

eval :: Value -> Env -> Maybe (Value, Env)
eval (Symbol s) e = (, e) <$> refer s e
eval (Cons f as) e = uncurry (flip apply as) =<< eval f e
eval v e = Just (v, e)

apply :: Value -> Value -> Env -> Maybe (Value, Env)
apply (Syntax _ s) as e = s as e
apply (Subroutine _ s) as e = uncurry s =<< evals as e
apply (Lambda _ ps bd) as e = evals as e >>= \(as', e') ->
	second (const e') <$> (begin bd =<< argument ps as' e')
apply _ _ _ = Nothing

evals :: Value -> Env -> Maybe (Value, Env)
evals Nil e = Just (Nil, e)
evals (v `Cons` vs) e = eval v e >>= uncurry (<$>) . (first . Cons *** evals vs)
evals _ _ = Nothing

argument :: Value -> Value -> Env -> Maybe Env
argument (Cons (Symbol p) ps) (Cons a as) e = set p a <$> argument ps as e
argument Nil Nil e = Just e
argument _ _ _ = Nothing

begin :: Value -> Env -> Maybe (Value, Env)
begin (v `Cons` Nil) e = eval v e
begin (v `Cons` vs) e = begin vs . snd =<< eval v e
begin _ _ = Nothing
