{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ShowProgress where

infixr 9 :.:
data FunList a b
	= Fun (a -> b)
	| forall x . Show x => (x -> b) :.: FunList a x

apply :: FunList a b -> a -> b
apply (Fun f) = f
apply (f :.: fs) = f . apply fs

showProgress :: Show a => FunList a b -> a -> (b, [String])
showProgress (Fun f) x = (f x, [show x])
showProgress (f :.: fs) x = (f y, show y : ps)
	where (y, ps) = showProgress fs x
