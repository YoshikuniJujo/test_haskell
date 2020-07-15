{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.Bool
import Numeric

import Field

tryField :: IO ()
tryField = do
	f <- openField "テスト" [exposureMask, keyPressMask]
	loop f
	closeField f

loop :: Field -> IO ()
loop f = do
	c <- withNextEvent f \case
		ke@KeyEvent {} -> True <$ do
			print ke
			putStrLn . (`showHex` "") =<< keycodeToKeysym f (ev_keycode ke) 0
			putStrLn . (`showHex` "") =<< keycodeToKeysym f (ev_keycode ke) 1
			putStrLn . (`showHex` "") =<< keycodeToKeysym f (ev_keycode ke) 2
			putStrLn . (`showHex` "") =<< keycodeToKeysym f (ev_keycode ke) 3
		ev	| isDeleteEvent f ev -> pure False
			| otherwise -> True <$ print ev
	bool (pure ()) (loop f) c
