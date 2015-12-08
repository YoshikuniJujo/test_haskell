{-# LANGUAGE TupleSections #-}

module Eval (evaluate, eval) where

import Environment
import Maybe

evaluate :: [Value] -> Env -> Maybe ([Value], Env)
evaluate [] e = Just ([], e)
evaluate (v : vs) e = case eval v e of
	Just (v', e') -> (\(vs', e'') -> (v' : vs', e'')) `mapply` evaluate vs e'
	_ -> Nothing

eval :: Value -> Env -> Maybe (Value, Env)
eval (Symbol s) e = (, e) `mapply` refer s e
eval b@(Bool _) e = Just (b, e)
eval i@(Int _) e = Just (i, e)
eval (List (v : vs)) e = (\(f, e') -> apply f vs e') `mbind` eval v e
eval el@(List []) e = Just (el, e)
eval s@(Sntx _ _) e = Just (s, e)
eval s@(Subr _ _) e = Just (s, e)
eval l@(Lmbd _ _) e = Just (l, e)

apply :: Value -> [Value] -> Env -> Maybe (Value, Env)
apply (Sntx _ s) vs e = s vs e
apply (Subr _ s) vs e = (\(as, e') -> s as e') `mbind` evaluate vs e
apply (Lmbd ps bd) vs e = (\(r, _) -> (r, e)) `mapply`
	(eval bd . foldr (uncurry set) e $ zip ps vs)
apply _ _ _ = Nothing
