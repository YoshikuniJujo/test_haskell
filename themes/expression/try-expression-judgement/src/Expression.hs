{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Expression (
	Expression, num, var, (.+), (.-), reduct,
	includeVar, annihilation, variables) where

newtype Expression i v = Expression [Atom i v] deriving (Show, Eq)

data Atom i v = Num i | Var i v deriving (Show, Eq, Ord)

coeff :: Atom i v -> i
coeff (Num n) = n
coeff (Var n _) = n

divide :: Integral i => Atom i v -> i -> Atom i v
divide (Num n) m = Num $ n `div` m
divide (Var n v) m = Var (n `div` m) v

multi :: Integral i => Atom i v -> i -> Atom i v
multi (Num n) m = Num $ n * m
multi (Var n v) m = Var (n * m) v

multiple :: Integral i => Expression i v -> i -> Expression i v
multiple (Expression as) n = Expression $ (`multi` n) <$> as

num :: (Integral i, Ord v) => i -> Expression i v
num = Expression . (: []) . Num

var :: (Integral i, Ord v) => v -> Expression i v
var = Expression . (: []) . Var 1

(.+), (.-) :: (Integral i, Ord v) =>
	Expression i v -> Expression i v -> Expression i v
Expression as .+ Expression bs = Expression $ as ..+ bs
Expression as .- Expression bs = Expression $ as ..+ (negateAtom <$> bs)

(..+) :: (Ord v, Integral i) => [Atom i v] -> [Atom i v] -> [Atom i v]
[] ..+ bs = bs
as ..+ [] = as
(Num n : as) ..+ (Num m : bs)
	| n + m == 0 = as ..+ bs
	| otherwise = Num (n + m) : (as ..+ bs)
(Num n : as) ..+ bs = Num n : (as ..+ bs)
aa@(Var _ _ : _) ..+ (Num m : bs) = Num m : (aa ..+ bs)
aa@(Var n v : as) ..+ ba@(Var m w : bs)
	| v < w = Var n v : (as ..+ ba)
	| v > w = Var m w : (aa ..+ bs)
	| n + m == 0 = as ..+ bs
	| otherwise = Var (n + m) v : (as ..+ bs)

negateAtom :: Integral i => Atom i v -> Atom i v
negateAtom (Num n) = Num $ negate n
negateAtom (Var n v) = Var (negate n) v

signumAtom :: Integral i => Atom i v -> i
signumAtom (Num n) = signum n
signumAtom (Var n _) = signum n

reduct :: Integral i => Expression i v -> Expression i v
reduct (Expression []) = Expression []
reduct e@(Expression as@(a : _)) = Expression $ (`divide` (n * m)) <$> as
	where
	n = divisor e
	m = signumAtom a

divisor :: Integral i => Expression i v -> i
divisor = foldr gcd 0 . coefficients

coefficients :: Expression i v -> [i]
coefficients (Expression as) = coeff <$> as

includeVar :: Ord v => Expression i v -> v -> Bool
includeVar (Expression as_) v_ = incVar as_ v_
	where
	incVar [] _ = False
	incVar (Num _ : as) v0 = incVar as v0
	incVar (Var _ v : as) v0
		| v < v0 = incVar as v0
		| v == v0 = True
		| otherwise = False

coefficientOf :: Ord v => Expression i v -> v -> Maybe i
coefficientOf (Expression as_) = coeffOf as_
	where
	coeffOf [] _ = Nothing
	coeffOf (Num _ : as) v0 = coeffOf as v0
	coeffOf (Var i v : as) v0
		| v < v0 = coeffOf as v0
		| v == v0 = Just i
		| otherwise = Nothing

annihilation :: (Integral i, Ord v) => Expression i v -> Expression i v -> v -> Maybe (Expression i v)
annihilation e1 e2 v = case (coefficientOf e1 v, coefficientOf e2 v) of
	(Just n1, Just n2) -> Just . reduct $ (e1 `multiple` n2) .- (e2 `multiple` n1)
	_ -> Nothing

variables :: Expression i v -> [v]
variables (Expression as_) = vars as_
	where
	vars [] = []
	vars (Num _ : as) = vars as
	vars (Var _ v : as) = v : vars as
