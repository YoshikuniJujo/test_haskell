{-# LANGUAGE TupleSections #-}

import Control.Applicative

safeDivM :: Int -> Int -> Maybe Int
_ `safeDivM` 0 = Nothing
x `safeDivM` y = Just $ x `div` y

calcM :: Int -> Int -> Int -> Maybe Int
calcM a b c =
	a `safeDivM` b >>= \x ->
	x `safeDivM` c

data Try a = Error String | Success a deriving Show

instance Functor Try where
	fmap = (=<<) . (return .)

instance Applicative Try where
	pure = return
	tf <*> tx =
		tf >>= \f ->
		tx >>= \x ->
		return $ f x

instance Monad Try where
	return = Success
	Error em >>= _ = Error em
	Success x >>= f = f x

safeDivE :: Int -> Int -> Try Int
x `safeDivE` 0 = Error $ show x ++ " is divided by zero\n"
x `safeDivE` y = Success $ x `div` y

calcE :: Int -> Int -> Int -> Try Int
calcE a b c =
	a `safeDivE` b >>= \x ->
	x `safeDivE` c

newtype Calc a = Calc { runCalc :: Int -> (a, Int) }

instance Functor Calc where
	fmap = (=<<) . (return .)

instance Applicative Calc where
	pure = return
	sf <*> sx =
		sf >>= \f ->
		sx >>= \x ->
		return $ f x

instance Monad Calc where
	return = Calc . (,)
	m >>= f = Calc $ \s -> let (x, s') = runCalc m s in runCalc (f x) s'

mplus :: Int -> Calc ()
mplus x = Calc $ (() ,) . (+ x)

mrecall :: Calc Int
mrecall = Calc $ \s -> (s, s)

-- (3 * 4 + 2 * 5) * 7 = 154

calcS :: Calc Int
calcS =	return (3 * 4) >>=
	mplus >>= \_ ->
	return (2 * 5) >>=
	mplus >>= \_ ->
	mrecall >>= \x ->
	return (x * 7)
