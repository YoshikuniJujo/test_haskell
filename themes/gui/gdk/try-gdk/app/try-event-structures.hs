{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Data.Foldable
import System.Environment
import System.Console.GetOpt
import System.Exit

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.GdkDevice
import Graphics.Gdk.GdkDevice.GdkAxes
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.Windows.GdkModifierType
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures
import Graphics.Gdk.EventStructures.GdkKeySyms
import Graphics.Gdk.PropertiesAndAtoms.GdkAtom

main :: IO ()
main = do
	(ss, _as, es) <- getOpt Permute [
		optEvents, optAllEvents, optNoEventCompression ] <$> getArgs
	putStrLn `mapM_` es
	when (not $ null es) exitFailure
	print ss
	_dpy <- gdkDisplayOpen ""
	win <- gdkToplevelNew Nothing
		$ minimalGdkWindowAttr (optToEvents ss) 700 500
	gdkWindowSetEventCompression win $ optToEventCompression ss
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
		GdkEventGdkButtonPress (gdkEventButton -> e) -> True <$ do
			putStrLn "BUTTON PRESS"
			print e
			let	axes = gdkEventButtonAxes e
			printDeviceAxes (gdkEventButtonDevice e) axes
			maybe (pure ()) (`printDeviceAxes` axes)
				$ gdkEventButtonSourceDevice e
		GdkEventGdkButtonRelease (gdkEventButton -> e) -> True <$
			(putStrLn "BUTTON RELEASE" >> print e)
		GdkEventGdkDoubleButtonPress (gdkEventButton -> e) -> True <$
			(putStrLn "DOUBLE BUTTON PRESS" >> print e)
		GdkEventGdkTripleButtonPress (gdkEventButton -> e) -> True <$
			(putStrLn "TRIPLE BUTTON PRESS" >> print e)
		GdkEventGdkScroll (gdkEventScroll -> e) -> True <$
			(putStrLn "SCROLL" >> print e)
		GdkEventGdkMotionNotify (gdkEventMotion -> e) -> True <$ do
			putStrLn "MOTION NOTIFY"
			print e
			print . gdkModifierTypeSingleBitList
				$ gdkEventMotionState e
			let	axes = gdkEventMotionAxes e
			printDeviceAxes (gdkEventMotionDevice e) axes
			maybe (pure ()) (`printDeviceAxes` axes)
				$ gdkEventMotionSourceDevice e
		GdkEventGdkVisibilityNotify (gdkEventVisibility -> e) -> True <$
			(putStrLn "VISIBILITY NOTIFY" >> print e)
		GdkEventGdkEnterNotify (gdkEventCrossing -> e) -> True <$ do
			putStrLn "ENTER NOTIFY"
			print e
		GdkEventGdkLeaveNotify (gdkEventCrossing -> e) -> True <$ do
			putStrLn "LEAVE NOTIFY"
			print e
		GdkEventGdkFocusChange (gdkEventFocus -> e) -> True <$ print e
		GdkEventGdkConfigure (gdkEventConfigure -> e) -> True <$ print e
		GdkEventGdkPropertyNotify (gdkEventProperty -> e) -> True <$ do
			print e
			putStrLn =<< gdkAtomName (gdkEventPropertyAtom e)
		GdkEventGdkWindowState (gdkEventWindowState -> e) -> True <$ do
			print e
			print . gdkWindowStateList
				$ gdkEventWindowStateChangedMask e
			print . gdkWindowStateList
				$ gdkEventWindowStateNewWindowState e
		GdkEventGdkAny (gdkEventAny -> e) -> True <$ print e

printDeviceAxes :: IsGdkDevice d => d 'Pointer -> GdkAxes -> IO ()
printDeviceAxes d axes = do
	print =<< gdkDeviceGetName d
	aa <- gdkDeviceListAxes d
	for_ aa \a -> do
		putStr "\t"
		putStr =<< gdkAtomName a
		putStr ": "
		print =<< gdkDeviceGetAxisValue d axes a

mainLoop :: Int -> (forall s . GdkEvent s -> IO Bool) -> IO ()
mainLoop slp f = gdkWithEvent \case
	Nothing -> threadDelay slp >> mainLoop slp f
	Just e -> f e >>= \case False -> pure (); True -> mainLoop slp f

data OptSetting
	= OptEvents [GdkEventMaskSingleBit]
	| OptAllEvents
	| OptNoEventCompression
	deriving Show

optEvents, optAllEvents, optNoEventCompression :: OptDescr OptSetting
optEvents = Option ['e'] ["events"] (ReqArg (OptEvents . read) "EventMaskList")
	"Set event mask list"

optAllEvents = Option ['a'] ["all-events"] (NoArg OptAllEvents)
	"Set all event mask on"

optNoEventCompression = Option ['c'] ["no-event-compression"]
	(NoArg OptNoEventCompression) "Set no event comparession"

optToEvents :: [OptSetting] -> GdkEventMaskMultiBits
optToEvents [] = gdkEventMaskMultiBits [GdkKeyPressMask]
optToEvents (OptAllEvents : _) = GdkAllEventsMask
optToEvents (OptEvents es : _) = gdkEventMaskMultiBits es
optToEvents (_ : ss) = optToEvents ss

optToEventCompression :: [OptSetting] -> Bool
optToEventCompression [] = True
optToEventCompression (OptNoEventCompression : _) = False
optToEventCompression (_ : ss) = optToEventCompression ss
