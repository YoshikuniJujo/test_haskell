{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad

import Field

main :: IO ()
main = do
	f <- openField "あいうえお" []
	while $ withNextEvent f \case
		DestroyWindowEvent {} -> True <$ closeField f
		ev	| isDeleteEvent f ev -> False <$ destroyField f
			| otherwise -> pure True

while :: Monad m => m Bool -> m ()
while act = (`when` while act) =<< act
