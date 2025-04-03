{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples where

import Control.Applicative
import Control.Monad

import Eff
import Monads
import NonDet

readerSample :: Eff '[Reader Integer] Integer
readerSample = do
	e <- ask
	return $ 3 * e

safeDiv :: Member (Exc String) effs => Integer -> Integer -> Eff effs Integer
n `safeDiv` 0 = throwError $ show n ++ " is devided by 0"
n `safeDiv` m = pure $ n `div` m

safeDivSample :: (Member (State Integer) effs, Member (Exc String) effs) =>
	Eff effs Integer
safeDivSample = do
	(a :: Integer) <- get
	modify (subtract (5 :: Integer))
	modify (* (2 :: Integer))
	b <- get
	c <- 60 `safeDiv` b
	put a
	modify (subtract (3 :: Integer))
	d <- get
	e <- 250 `safeDiv` d
	pure $ c + e

runSafeDivSample1 :: Integer -> Either String (Integer, Integer)
runSafeDivSample1 n = run . runError $ safeDivSample `runState` n

runSafeDivSample2 :: Integer -> (Either String Integer, Integer)
runSafeDivSample2 n = run $ runError safeDivSample `runState` n

runMSample :: (Member (State Integer) effs, Member IO effs) => Eff effs ()
runMSample = do
	(a :: Integer) <- get
	inj (print a) `Bind` Pure

ifte :: Member NonDet r => Eff r a -> (a -> Eff r b) -> Eff r b -> Eff r b
ifte t th el = (t >>= th) <|> el

testIfte :: Member NonDet r => Eff r Int
testIfte = do
	n <- gen
	ifte	(do	d <- gen
			guard $ d < n && n `mod` d == 0)
		(const mzero)
		(pure n)
	where gen = msum . fmap pure $ [2 .. 30]

piyo :: Member NonDet r => Eff r (Int, Int)
piyo = do
	n <- gen
	m <- gen
	guard (n `mod` m == 0)
	pure (n, m)
	where gen = msum . fmap pure $ [2 .. 30]

testIfteRun :: [Int]
testIfteRun = run . makeChoiceA $ testIfte
