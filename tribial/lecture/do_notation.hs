{-# LANGUAGE TupleSections #-}

import Control.Applicative

newtype Calc a = Calc { runCalc :: Int -> (a, Int) }

instance Functor Calc where
	fmap = (=<<) . (return .)

instance Applicative Calc where
	pure = return
	mf <*> mx =
		mf >>= \f ->
		mx >>= \x ->
		return $ f x

instance Monad Calc where
	return = Calc . (,)
	m >>= f = Calc $ \s ->
		let (x, s') = runCalc m s in runCalc (f x) s'

mplus :: Int -> Calc ()
mplus x = Calc $ (() ,) . (+ x)

mrecall :: Calc Int
mrecall = Calc $ \s -> (s, s)

calc0 :: Calc Int
calc0 =
	return (3 * 4) >>=
	mplus >>
	return (2 * 5) >>=
	mplus >>
	mrecall >>=
	return . (* 7)

calc1 :: Calc Int
calc1 =
	return (3 * 4) >>= \x ->
	mplus x >>
	return (2 * 5) >>= \y ->
	mplus y >>
	mrecall >>= \z ->
	return (z * 7)

calc2 :: Calc Int
calc2 = do
	x <- return (3 * 4)
	mplus x
	y <- return (2 * 5)
	mplus y
	z <- mrecall
	return $ z * 7

calc3 :: Calc Int
calc3 = do
	let x = 3 * 4
	mplus x
	let y = 2 * 5
	z <- mrecall
	return $ z * 7

calc4 :: Calc Int
calc4 = do
	mplus $ 3 * 4
	mplus $ 2 * 5
	mrecall >>= return . (* 7)

calc5 :: Calc Int
calc5 = do
	mplus $ 3 * 4
	mplus $ 2 * 5
	fmap (* 7) mrecall

calc6 :: Calc Int
calc6 = do
	mplus $ 3 * 4
	mplus $ 2 * 5
	(* 7) <$> mrecall
