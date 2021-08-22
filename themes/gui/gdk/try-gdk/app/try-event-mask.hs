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
	let	opts = [optEvents, optAllEvents, optNoCompression]
	(ss, _as, es) <- getOpt Permute opts <$> getArgs
	putStrLn `mapM_` es
	print ss
	_ <- gdkDisplayOpen ""
	win <- gdkToplevelNew Nothing $ minimalGdkWindowAttr
		(optsToEventMask ss) 400 300
	gdkWindowSetEventCompression win $ optsToCompression ss
	gdkWindowShow win
	mainLoop \case
		GdkEventGdkNothing (gdkEventAny -> e) -> True <$ print e
		GdkEventGdkDelete (gdkEventAny -> e) -> False <$ (print e >> gdkWindowDestroy win)
		GdkEventGdkDestroy (gdkEventAny -> e) -> False <$ print e
		GdkEventGdkMotionNotify (gdkEventMotion -> e) -> True <$ print e
		GdkEventGdkKeyPress
			(gdkEventKeyKeyval . gdkEventKey -> GdkKey_q) ->
				False <$ gdkWindowDestroy win
		GdkEventGdkAny (gdkEventAny -> e) -> True <$ print e

data OptSetting
	= OptEvents [GdkEventMaskSingleBit]
	| OptAllEvents
	| OptNoCompression
	deriving Show

optEvents, optAllEvents, optNoCompression :: OptDescr OptSetting
optEvents = Option ['e'] ["events"] (ReqArg (OptEvents . read) "EventMask")
	"Set event mask"

optAllEvents = Option ['a'] ["all-events"] (NoArg OptAllEvents) "Set all event mask"

optNoCompression = Option ['c'] ["no-compression"] (NoArg OptNoCompression)
	"Set no compression"

optsToEventMask :: [OptSetting] -> GdkEventMaskMultiBits
optsToEventMask = \case
	[] -> gdkEventMaskMultiBits [GdkKeyPressMask]
	OptEvents ems : _ -> gdkEventMaskMultiBits ems
	OptAllEvents : _ -> GdkAllEventsMask

optsToCompression :: [OptSetting] -> Bool
optsToCompression [] = True
optsToCompression (OptNoCompression : _) = False
optsToCompression (_ : ss) = optsToCompression ss
