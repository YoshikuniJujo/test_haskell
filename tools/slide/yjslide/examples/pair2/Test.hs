{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Test (main, TPair, WPair, Word32) where

import Pair
import System.Random
import Control.Monad

getRandomPairs :: Pairable p Word32 => IO [p Word32]
getRandomPairs = replicateM (10 ^ 4) $ do
	w1 <- randomIO
	w2 <- randomIO
	return $ pair w1 w2

lookupPair :: Pairable p Word32 => Word32 -> [p Word32] -> Maybe Word32
lookupPair _ [] = Nothing
lookupPair w0 (wp : wps)
	| (w1, w2) <- unpair wp, abs (w1 - w0) < 1000 = Just w2
	| otherwise = lookupPair w0 wps

timesDo :: Int -> IO () -> IO ()
0 `timesDo` _ = return ()
n `timesDo` io = io >> (n - 1) `timesDo` io

main :: Pairable p Word32 => p Word32 -> IO ()
main (_ :: p Word32) = do
	ps <- getRandomPairs :: IO [p Word32]
	(10 ^ 3) `timesDo` (do
		k <- randomIO
		case lookupPair k ps of
			Just v -> print v
			_ -> return ())
