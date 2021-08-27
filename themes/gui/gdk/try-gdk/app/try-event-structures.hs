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
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms

main :: IO ()
main = do
	(ss, _as, es) <- getOpt Permute [optEvents] <$> getArgs
	putStrLn `mapM_` es
	when (not $ null es) exitFailure
	print ss
	_dpy <- gdkDisplayOpen ""
	win <- gdkToplevelNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits (optToEvents ss)) 700 500
	gdkWindowShow win
	mainLoop 100000 \case
		GdkEventGdkNothing (gdkEventAny -> e) -> True <$
			(putStrLn "NOTHING" >> print e)
		GdkEventGdkDelete (gdkEventAny -> e) -> True <$
			(putStrLn "DELETE" >> print e >> gdkWindowDestroy win)
		GdkEventGdkDestroy (gdkEventAny -> e) -> False <$
			(putStrLn "DESTROY" >> print e)
--		GdkEventGdkKeyPress (gdkEventKey -> e) -> True <$ print e
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> GdkKey_q) ->
			pure False
		GdkEventGdkAny (gdkEventAny -> e) -> True <$ print e

mainLoop :: Int -> (forall s . GdkEvent s -> IO Bool) -> IO ()
mainLoop slp f = gdkWithEvent \case
	Nothing -> threadDelay slp >> mainLoop slp f
	Just e -> f e >>= \case False -> pure (); True -> mainLoop slp f

data OptSetting = OptEvents [GdkEventMaskSingleBit] deriving Show

optEvents :: OptDescr OptSetting
optEvents = Option ['e'] ["events"] (ReqArg (OptEvents . read) "EventMaskList")
	"Set event mask list"

optToEvents :: [OptSetting] -> [GdkEventMaskSingleBit]
optToEvents [] = [GdkKeyPressMask]
optToEvents (OptEvents es : _) = es
