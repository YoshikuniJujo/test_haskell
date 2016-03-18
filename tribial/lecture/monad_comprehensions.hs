{-# LANGUAGE MonadComprehensions, TupleSections #-}

import Control.Applicative

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just $ x `div` y

calcM0, calcM :: Int -> Int -> Int -> Maybe Int
calcM0 a b c = do
	x <- a `safeDiv` b
	y <- x `safeDiv` c
	return y

calcM a b c = [ y | x <- a `safeDiv` b, y <- x `safeDiv` c ]

newtype Calc a = Calc { runCalc :: Int -> (a, Int) }

instance Functor Calc where
	fmap = (=<<) . (return .)

instance Applicative Calc where
	pure = return
	mf <*> mx = do
		f <- mf
		x <- mx
		return $ f x

instance Monad Calc where
	return = Calc . (,)
	m >>= f = Calc $ \s ->
		let (x, s') = runCalc m s in runCalc (f x) s'

mplus :: Int -> Calc ()
mplus x = Calc $ (() ,) . (+ x)

mrecall :: Calc Int
mrecall = Calc $ \s -> (s, s)

calcC0, calcC :: Calc Int
calcC0 = do
 	mplus $ 3 * 4
	mplus $ 2 * 5
	x <- mrecall
	return $ x * 7

calcC = [ x * 7 | _ <- mplus $ 3 * 4, _ <- mplus $ 2 * 5, x <- mrecall ]
