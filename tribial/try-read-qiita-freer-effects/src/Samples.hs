{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Samples where

import Eff
import Monads

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
