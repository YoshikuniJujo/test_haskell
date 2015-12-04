{-# LANGUAGE TupleSections #-}

module Primitive (env0) where

import Control.Applicative ((<$>), (<*>))
import Data.List (foldl')

import Eval (eval)
import Environment (
	Env, fromList, set, Value(..), showValue, ErrMsg, stxErr, prpErr)

env0 :: Env
env0 = fromList [
	("define", Syntax "define" define), ("lambda", Syntax "lambda" lambda),
	("if", Syntax "if" ifs),
	("+", Subroutine "+" add), ("-", Subroutine "-" sub),
	("*", Subroutine "*" mul), ("<", Subroutine "<" ltt) ]

define, lambda, ifs :: Value -> Env -> Either ErrMsg (Value, Env)
define (Cons a@(Symbol s) (Cons v Nil)) e = (a ,) . uncurry (set s) <$> eval v e
define (Cons (Cons f@(Symbol n) as) bd) e = Right (f, set n (Lambda n as bd) e)
define v _ = Left $ stxErr ++ showValue (Symbol "define" `Cons` v)

lambda (Cons as bd) e = Right (Lambda "#f" as bd, e)
lambda v _ = Left $ stxErr ++ showValue (Symbol "lambda" `Cons` v)

ifs (Cons p (Cons th (Cons el Nil))) e = eval p e >>= \(b, e') ->
	case b of B False -> eval el e'; _ -> eval th e'
ifs v _ = Left $ stxErr ++ showValue (Symbol "if" `Cons` v)

nums :: Value -> Either ErrMsg [Integer]
nums Nil = Right []
nums (Int i `Cons` vs) = (i :) <$> nums vs
nums v = Left $ prpErr ++ showValue v

add, mul, sub, ltt :: Value -> Env -> Either ErrMsg (Value, Env)
add v e = (, e) . Int . sum <$> nums v
mul v e = (, e) . Int . product <$> nums v
sub v e = (, e) . Int . sb <$> nums v
	where sb [x] = - x; sb (x : xs) = foldl' (-) x xs; sb _ = 0
ltt v e = (, e) . B . and . (zipWith (<) <$> id <*> tail) <$> nums v
