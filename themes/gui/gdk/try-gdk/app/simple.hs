{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Data.Bool
import Data.Char
import System.Environment

import Graphics.Gdk.General
import Graphics.Gdk.Windows
import Graphics.Gdk.Event
import Graphics.Gdk.Types
import Graphics.Gdk.Values

main :: IO ()
main = do
	print =<< join (gdkInit <$> getProgName <*> getArgs)
	let	wattr = mkGdkWindowAttr [
				gdkExposureMask, gdkButtonPressMask, gdkKeyPressMask ]
			400 400
			gdkInputOutput gdkWindowToplevel
	w <- gdkWindowNew Nothing wattr
	gdkWindowShow w
	doWhile_ do
		threadDelay 100000
		doWhile $ gdkEventGet >>= \case
			Just e -> do
				b <- checkEvent e
				pure if b then Nothing else Just False
			Nothing -> pure $ Just True

checkEvent :: GdkEvent -> IO Bool
checkEvent = \case
	GdkEventGdkDelete d -> pure False
	GdkEventGdkKeyPress k -> do
		kv <- gdkEventKeyKeyval k
		pure $ kv /= fromIntegral (ord 'q')
	e -> True <$ print e

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ act = bool (pure ()) (doWhile_ act) =<< act

doWhile :: Monad m => m (Maybe a) -> m a
doWhile act = maybe (doWhile act) pure =<< act
