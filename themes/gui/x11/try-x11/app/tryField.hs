{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad

import Field
import ButtonEvent

main :: IO ()
main = do
	f <- openField "あいうえお" [
		exposureMask, buttonPressMask, buttonReleaseMask ]
	while $ withNextEvent f \case
		DestroyWindowEvent {} -> False <$ closeField f
		ExposeEvent {} -> True <$ (fillRect f 0xff0000 150 100 300 200 >> flushField f)
		ev@ButtonEvent {} -> True <$ do
			maybe (pure ()) print $ buttonEvent ev
		ev	| isDeleteEvent f ev -> True <$ destroyField f
			| otherwise -> print ev >> pure True

while :: Monad m => m Bool -> m ()
while act = (`when` while act) =<< act
