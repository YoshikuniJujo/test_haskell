{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Examples where

import Eff
import State
import Exception

safeDiv :: Member (Exc String) effs => Integer -> Integer -> Eff effs Integer
n `safeDiv` 0 = throwError $ show n ++ " is devided by 0"
n `safeDiv` m = return $ n `div` m

sample1 :: (Member (State Integer) effs, Member (Exc String) effs) =>
	Eff effs Integer
sample1 = do
	(a :: Integer) <- get
--	a <- get
	modify (subtract (5 :: Integer))
	modify (* (2 :: Integer))
	b <- get
	c <- 60 `safeDiv` b
	put a
	modify (subtract (3 :: Integer))
	d <- get
	e <- 250 `safeDiv` d
	return $ c + e

runSample1_1 :: Integer -> Either String (Integer, Integer)
runSample1_1 n = run $ runError (sample1 `runState` n)

runSample1_2 :: Integer -> (Either String Integer, Integer)
runSample1_2 n = run $ runError sample1 `runState` n
