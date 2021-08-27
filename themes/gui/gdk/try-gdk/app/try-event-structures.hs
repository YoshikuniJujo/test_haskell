{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import System.Environment
import System.Console.GetOpt
import System.Exit

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.Windows.GdkModifierType
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms

main :: IO ()
main = do
	(ss, _as, es) <- getOpt Permute [optEvents, optAllEvents] <$> getArgs
	putStrLn `mapM_` es
	when (not $ null es) exitFailure
	print ss
	_dpy <- gdkDisplayOpen ""
	win <- gdkToplevelNew Nothing
		$ minimalGdkWindowAttr (optToEvents ss) 700 500
	gdkWindowShow win
	mainLoop 100000 \case
		GdkEventGdkNothing (gdkEventAny -> e) -> True <$
			(putStrLn "NOTHING" >> print e)
		GdkEventGdkDelete (gdkEventAny -> e) -> False <$
			(putStrLn "DELETE" >> print e >> gdkWindowDestroy win)
		GdkEventGdkDestroy (gdkEventAny -> e) -> False <$
			(putStrLn "DESTROY" >> print e)
		GdkEventGdkMap (gdkEventAny -> e) -> True <$
			(putStrLn "MAP" >> print e)
		GdkEventGdkUnmap (gdkEventAny -> e) -> True <$
			(putStrLn "UNMAP" >> print e)
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> GdkKey_q) ->
			pure False
		GdkEventGdkKeyPress (gdkEventKey -> e) -> True <$ do
			putStrLn "KEY PRESS"
			print e
			print . gdkModifierTypeSingleBitList $ gdkEventKeyState e
		GdkEventGdkKeyRelease (gdkEventKey -> e) -> True <$ do
			putStrLn "KEY RELEASE"
			print e
			print . gdkModifierTypeSingleBitList $ gdkEventKeyState e
		GdkEventGdkAny (gdkEventAny -> e) -> True <$ print e

mainLoop :: Int -> (forall s . GdkEvent s -> IO Bool) -> IO ()
mainLoop slp f = gdkWithEvent \case
	Nothing -> threadDelay slp >> mainLoop slp f
	Just e -> f e >>= \case False -> pure (); True -> mainLoop slp f

data OptSetting = OptEvents [GdkEventMaskSingleBit] | OptAllEvents deriving Show

optEvents, optAllEvents :: OptDescr OptSetting
optEvents = Option ['e'] ["events"] (ReqArg (OptEvents . read) "EventMaskList")
	"Set event mask list"

optAllEvents = Option ['a'] ["all-events"] (NoArg OptAllEvents)
	"Set all event mask on"

optToEvents :: [OptSetting] -> GdkEventMaskMultiBits
optToEvents [] = gdkEventMaskMultiBits [GdkKeyPressMask]
optToEvents (OptAllEvents : _) = GdkAllEventsMask
optToEvents (OptEvents es : _) = gdkEventMaskMultiBits es
