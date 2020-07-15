{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.Bool

import Field

tryField :: IO ()
tryField = do
	f <- openField "テスト" [exposureMask, keyPressMask]
	loop f
	closeField f

loop :: Field -> IO ()
loop f = do
	c <- withNextEvent f \case
		ev	| isDeleteEvent f ev -> pure False
			| otherwise -> True <$ print ev
	bool (pure ()) (loop f) c
