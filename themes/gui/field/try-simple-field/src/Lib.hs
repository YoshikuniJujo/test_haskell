{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.Bool
import System.IO
import Numeric

import Field
import Graphics.X11 (keysymToString, KeySym)

tryField :: FilePath -> IO ()
tryField fp = do
	f <- openField "テスト" [exposureMask, keyPressMask, keyReleaseMask, buttonPressMask]
	h <- openFile fp WriteMode
	loop f h
	hClose h
	closeField f

loop :: Field -> Handle -> IO ()
loop f h = do
	c <- withNextEvent f \case
		(ke@KeyEvent {}, _) -> True <$ do
			print ke
			putStrWd . formatKeySym =<< keycodeToKeysym f (ev_keycode ke) 0
			putStrLn . formatKeySym =<< keycodeToKeysym f (ev_keycode ke) 1

			hPutStrWd h . formatKeySym =<< keycodeToKeysym f (ev_keycode ke) 0
			hPutStrWd h . formatKeySym =<< keycodeToKeysym f (ev_keycode ke) 1
{-
			putStrWd . (`showHex` "") =<< keycodeToKeysym f (ev_keycode ke) 0
			putStrWd . (`showHex` "") =<< keycodeToKeysym f (ev_keycode ke) 1
			putStrWd . keysymToString =<< keycodeToKeysym f (ev_keycode ke) 0
			putStrLn . keysymToString =<< keycodeToKeysym f (ev_keycode ke) 1
-}
		(ButtonEvent {}, _) -> True <$ (putStrLn "" >> hPutStrLn h "")
		(ev, _)	| isDeleteEvent f ev -> pure False
			| otherwise -> True <$ print ev
	bool (pure ()) (loop f h) c

putStrWd :: String -> IO ()
putStrWd = putStr . (++ " ")

hPutStrWd :: Handle -> String -> IO ()
hPutStrWd h = hPutStr h . (++ " ")

formatKeySym :: KeySym -> String
formatKeySym s = keysymToString s ++ " (" ++ s `showHex` "" ++ ")"
