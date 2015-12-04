{-# LANGUAGE TupleSections #-}

module Primitive (env0) where

import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Data.List (foldl')

import Eval (eval)
import Environment (
	Env, fromList, set, Value(..), showValue, ErrMsg,
	syntaxErr, notNumErr, prpLstErr, lstOneErr, strReqErr, noApplErr)

env0 :: Env
env0 = fromList [
	("define", Syntax "define" define),
	("lambda", Syntax "lambda" lambda),
	("if", Syntax "if" ifs),
	("+", Subroutine "+" add),
	("-", Subroutine "-" sub),
	("*", Subroutine "*" mul),
	("/", Subroutine "/" divs),
	("<", Subroutine "<" ltt),
	("string-append", Subroutine "string-append" stringAppend),
	("x->string", Subroutine "x->string" x2string) ]

define, lambda, ifs :: Value -> Env -> Either ErrMsg (Value, Env)
define (Cons a@(Symbol s) (Cons v Nil)) e = (a ,) . uncurry (set s) <$> eval v e
define (Cons (Cons f@(Symbol n) as) bd) e = Right (f, set n (Lambda n as bd) e)
define v _ = Left $ syntaxErr ++ showValue (Symbol "define" `Cons` v)

lambda (Cons as bd) e = Right (Lambda "#f" as bd, e)
lambda v _ = Left $ syntaxErr ++ showValue (Symbol "lambda" `Cons` v)

ifs (Cons p (Cons th (Cons el Nil))) e = eval p e >>= \(b, e') ->
	case b of Bool False -> eval el e'; _ -> eval th e'
ifs v _ = Left $ syntaxErr ++ showValue (Symbol "if" `Cons` v)

list :: Value -> Either ErrMsg [Value]
list Nil = Right []; list (v `Cons` vs) = (v :) <$> list vs
list v = Left $ prpLstErr ++ showValue v

nums :: [Value] -> Either ErrMsg (Either [Integer] [Double])
nums [] = Right $ Left []
nums (Int i : s) = either (Left . (i :)) (Right . (fromIntegral i :)) <$> nums s
nums (Dbl d : vs) = Right . (d :) . either (map fromIntegral) id <$> nums vs
nums (v : _) = Left $ notNumErr ++ showValue v

add, mul, sub, divs, ltt :: Value -> Env -> Either ErrMsg (Value, Env)
add v e = (, e) . either (Int . sum) (Dbl . sum) <$> (nums =<< list v)
mul v e = (, e) . either (Int . product) (Dbl . product) <$> (nums =<< list v)
sub v e = (, e) . either (Int . sb) (Dbl . sb) <$> (ck =<< nums =<< list v)
	where
	ck s | either null null s = Left lstOneErr | otherwise = Right s
	sb [x] = negate x; sb (x : xs) = foldl' (-) x xs; sb _ = error "never"
divs v e = (, e) . Dbl . dv . either (map fromIntegral) id
	<$> (ck =<< nums =<< list v)
	where
	ck s | either null null s = Left lstOneErr | otherwise = Right s
	dv [x] = recip x; dv (x : xs) = foldl' (/) x xs; dv _ = error "never"
ltt v e = (, e) . Bool . and
	. either (zipWith (<) <$> id <*> tail) (zipWith (<) <$> id <*> tail)
	<$> (nums =<< list v)

stringAppend, x2string :: Value -> Env -> Either ErrMsg (Value, Env)
stringAppend (Cons (Str str) strs) e = first (sa str) <$> stringAppend strs e
	where sa s (Str ss) = Str $ s ++ ss; sa _ _ = error "never occur"
stringAppend Nil e = Right (Str "", e)
stringAppend v _ = Left $ strReqErr ++ showValue v

x2string (Cons v Nil) e = Right (Str $ showValue v, e)
x2string v _ = Left $ noApplErr ++ showValue v
