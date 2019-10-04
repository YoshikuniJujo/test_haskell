{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Examples where

import Control.Monad

import Eff
import State as St
import Reader
import Exception
import Iteratee as It

safeDiv :: Member (Exc String) effs => Integer -> Integer -> Eff effs Integer
n `safeDiv` 0 = throwError $ show n ++ " is devided by 0"
n `safeDiv` m = return $ n `div` m

sample1 :: (Member (State Integer) effs, Member (Exc String) effs) =>
	Eff effs Integer
sample1 = do
	(a :: Integer) <- St.get
--	a <- get
	modify (subtract (5 :: Integer))
	modify (* (2 :: Integer))
	b <- St.get
	c <- 60 `safeDiv` b
	put a
	modify (subtract (3 :: Integer))
	d <- St.get
	e <- 250 `safeDiv` d
	return $ c + e

runSample1_1 :: Integer -> Either String (Integer, Integer)
runSample1_1 n = run $ runError (sample1 `runState` n)

runSample1_2 :: Integer -> (Either String Integer, Integer)
runSample1_2 n = run $ runError sample1 `runState` n

sumInputL, sumInputR :: Member (Iteratee Int) effs => Int -> Int -> Eff effs Int
sumInputL n = foldl (>=>) return (replicate (n - 1) $ (<$> It.get) . (+))
sumInputR n = foldr (>=>) return (replicate (n - 1) $ (<$> It.get) . (+))

testSumInputL, testSumInputR :: Int -> Maybe Int
testSumInputL n = run $ sumInputL n 1 `runIteratee` [2 .. n]
testSumInputR n = run $ sumInputR n 1 `runIteratee` [2 .. n]

sumReaderL, sumReaderR :: Member (Reader Int) effs => Int -> Int -> Eff effs Int
sumReaderL n = foldl (>=>) return (replicate (n - 1) $ (<$> ask) . (+))
sumReaderR n = foldr (>=>) return (replicate (n - 1) $ (<$> ask) . (+))

testSumReaderL, testSumReaderR :: Int -> Int
testSumReaderL n = run $ sumReaderL n 10 `runReader` (10 :: Int)
testSumReaderR n = run $ sumReaderR n 10 `runReader` (10 :: Int)
