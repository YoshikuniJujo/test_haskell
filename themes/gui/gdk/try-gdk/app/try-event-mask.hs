{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import System.Environment
import System.Console.GetOpt

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms

import Try.Tools

main :: IO ()
main = do
	let	opts = [optEvents, optAllEvents]
	(ss, _as, es) <- getOpt Permute opts <$> getArgs
	putStrLn `mapM_` es
	print ss
	_ <- gdkDisplayOpen ""
	win <- gdkToplevelNew Nothing $ minimalGdkWindowAttr
		(optsToEventMask ss) 400 300
	gdkWindowShow win
	mainLoop \case
		GdkEventGdkNothing (gdkEventAny -> e) -> True <$ print e
		GdkEventGdkDelete (gdkEventAny -> e) -> False <$ (print e >> gdkWindowDestroy win)
		GdkEventGdkDestroy (gdkEventAny -> e) -> False <$ print e
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> GdkKey_q) ->
				False <$ gdkWindowDestroy win
		GdkEventGdkAny (gdkEventAny -> e) -> True <$ print e

data OptSetting
	= OptEvents [GdkEventMaskSingleBit]
	| OptAllEvents
	deriving Show

optEvents, optAllEvents :: OptDescr OptSetting
optEvents = Option ['e'] ["events"] (ReqArg (OptEvents . read) "EventMask")
	"Set event mask"

optAllEvents = Option ['a'] ["all-events"] (NoArg OptAllEvents) "Set all event mask"

optsToEventMask :: [OptSetting] -> GdkEventMaskMultiBits
optsToEventMask = \case
	[] -> gdkEventMaskMultiBits [GdkKeyPressMask]
	OptEvents ems : _ -> gdkEventMaskMultiBits ems
	OptAllEvents : _ -> GdkAllEventsMask
