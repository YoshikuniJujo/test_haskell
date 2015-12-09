{-# LANGUAGE TupleSections #-}

module Eval (eval, evaluate) where

import Environment
import Maybe

evaluate :: [Value] -> Env -> Maybe ([Value], Env)
evaluate [] e = Just ([], e)
evaluate (v : vs) e = case eval v e of
	Just (v', e') -> (\(vs', e'') -> (v' : vs', e''))
		`mapply` evaluate vs e'
	_ -> Nothing

eval :: Value -> Env -> Maybe (Value, Env)
eval (Sym s) e = (, e) `mapply` refer s e
eval (List (v : vs)) e = (\(f, e') -> apply f vs e') `mbind` eval v e
eval v e = Just (v, e)

apply :: Value -> [Value] -> Env -> Maybe (Value, Env)
apply (Sntx s) vs e = s vs e
apply (Subr s) vs e = (\(as, e') -> s as e') `mbind` evaluate vs e
apply _ _ _ = Nothing
