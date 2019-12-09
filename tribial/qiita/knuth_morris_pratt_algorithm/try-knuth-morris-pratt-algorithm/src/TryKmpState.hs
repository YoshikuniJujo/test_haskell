{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryKmpState where

import KnuthMorrisPrattAlgorithm

checkMatch :: Eq a => KmpState a -> [a] -> Maybe (KmpState a)
checkMatch st [] = if found st then Nothing else Just st
checkMatch st (x : xs)
	| found st = Nothing
	| otherwise = checkMatch (st `nextState` x) xs

checkLines :: KmpState Char -> IO ()
checkLines st = do
	cs <- getLine
	case checkMatch st cs of
		Nothing -> putStrLn "found"
		Just st' -> checkLines st'
