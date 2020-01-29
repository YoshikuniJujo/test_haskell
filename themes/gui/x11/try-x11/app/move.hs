{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Data.Foldable

import Field

main :: IO ()
main = do
	f <- openField "動くよ" [exposureMask]
	forkIO $ for_ [0 ..] \x -> do
--		fillRect f 0x00ff00 (100 + 5 * (x `mod` 40)) 100 300 200
		fillRect f 0x00ff00 (100 + vibration x) 100 300 200
		flushField f
		threadDelay 10000
		clearField f
	while $ withNextEvent f \case
		DestroyWindowEvent {} -> False <$ closeField f
--		ExposeEvent {} -> True <$ fillRect f 0x00ff00 150 100 300 200
		ev	| isDeleteEvent f ev -> True <$ destroyField f
			| otherwise -> print ev >> pure True

while :: Monad m => m Bool -> m ()
while act = (`when` while act) =<< act

vibration :: Position -> Position
vibration t_ = round . vb $ fromIntegral t_
	where
	vb :: Double -> Double
	vb t = 8 * cos (t / 12)
