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
import Graphics.Gdk.PropertiesAndAtoms.GdkAtom

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
		GdkEventGdkNothing e -> True <$ (print =<< gdkEventAny e)
		GdkEventGdkDelete e -> False <$ ((print =<< gdkEventAny e) >> gdkWindowDestroy win)
		GdkEventGdkDestroy e -> False <$ (print =<< gdkEventAny e)
		GdkEventGdkMotionNotify (gdkEventMotion -> e) -> True <$ print e
		GdkEventGdkButtonPress e -> True <$ (print =<< gdkEventButton e)
		GdkEventGdkButtonRelease e -> True <$ (print =<< gdkEventButton e)
		GdkEventGdkDoubleButtonPress e -> True <$ do
			putStrLn "DOUBLE"
			print =<< gdkEventButton e
		GdkEventGdkTripleButtonPress e -> True <$ do
			putStrLn "TRIPLE"
			print =<< gdkEventButton e
		GdkEventGdkKeyPress e -> do
			k <- gdkEventKey e
			print k
			pure case gdkEventKeyKeyval k of
				GdkKey_q -> False; _ -> True
		GdkEventGdkKeyRelease e -> True <$ do
			putStrLn "RELEASE"
			print =<< gdkEventKey e
		GdkEventGdkEnterNotify (gdkEventCrossing -> e) -> True <$ do
			putStrLn "ENTER"
			print e
		GdkEventGdkLeaveNotify (gdkEventCrossing -> e) -> True <$ do
			putStrLn "LEAVE"
			print e
		GdkEventGdkFocusChange (gdkEventFocus -> e) -> True <$ print e
		GdkEventGdkConfigure (gdkEventConfigure -> e) -> True <$ print e
		GdkEventGdkPropertyNotify (gdkEventProperty -> e) -> True <$ do
			print e
			putStrLn =<< gdkAtomName (gdkEventPropertyAtom e)
		GdkEventGdkVisibilityNotify (gdkEventVisibility -> e) ->
			True <$ print e
		GdkEventGdkScroll (gdkEventScroll -> e) -> True <$ print e
		GdkEventGdkAny e -> True <$ (print =<< gdkEventAny e)

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
	_ : ss -> optsToEventMask ss

optsToCompression :: [OptSetting] -> Bool
optsToCompression [] = True
optsToCompression (OptNoCompression : _) = False
optsToCompression (_ : ss) = optsToCompression ss
