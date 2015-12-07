{-# LANGUAGE TupleSections #-}

module Eval (evaluate) where

import Environment
import Maybe

evaluate :: [Value] -> Env -> Maybe ([Value], Env)
evaluate [] e = Just ([], e)
evaluate (v : vs) e = case eval v e of
	Just (v', e') -> (\(vs', e'') -> (v' : vs', e'')) `mapply` evaluate vs e'
	_ -> Nothing

eval :: Value -> Env -> Maybe (Value, Env)
eval (Symbol s) e = (, e) `mapply` refer s e
eval i@(Int _) e = Just (i, e)
eval (List (v : vs)) e = (\(f, e') -> apply f vs e') `mbind` eval v e
eval el@(List []) e = Just (el, e)
eval s@(Subr _ _) e = Just (s, e)

apply :: Value -> [Value] -> Env -> Maybe (Value, Env)
apply (Subr _ s) vs e = (\(as, e') -> s as e') `mbind` evaluate vs e
apply _ _ _ = Nothing
