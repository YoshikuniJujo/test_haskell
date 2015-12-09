{-# LANGUAGE TupleSections #-}

module Eval (eval, evaluate) where

import Environment
import Maybe

eval :: Value -> Env -> Maybe (Value, Env)
eval (Sym s) e = (, e) `mapply` refer s e
eval v e = Just (v, e)

evaluate :: [Value] -> Env -> Maybe ([Value], Env)
evaluate [] e = Just ([], e)
evaluate (v : vs) e = case eval v e of
	Just (v', e') -> (\(vs', e'') -> (v' : vs', e''))
		`mapply` evaluate vs e'
	_ -> Nothing
