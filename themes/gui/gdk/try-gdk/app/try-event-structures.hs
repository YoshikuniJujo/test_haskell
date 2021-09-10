{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Data.Foldable
import Data.KeySym
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
		GdkEventGdkNothing e -> True <$
			(putStrLn "NOTHING" >> (print =<< gdkEventAny e))
		GdkEventGdkDelete e -> False <$
			(putStrLn "DELETE" >> (print =<< gdkEventAny e) >> gdkWindowDestroy win)
		GdkEventGdkDestroy e -> False <$
			(putStrLn "DESTROY" >> (print =<< gdkEventAny e))
		GdkEventGdkMap e -> True <$
			(putStrLn "MAP" >> (print =<< gdkEventAny e))
		GdkEventGdkUnmap e -> True <$
			(putStrLn "UNMAP" >> (print =<< gdkEventAny e))
		GdkEventGdkKeyPress e -> do
			putStrLn "KEY PRESS"
			k <- gdkEventKey e
			print k
			print . gdkModifierTypeSingleBitList $ gdkEventKeyState k
			pure case gdkEventKeyKeyval k of
				Xk_q -> False; _ -> True
		GdkEventGdkKeyRelease e -> True <$ do
			putStrLn "KEY RELEASE"
			print =<< gdkEventKey e
			print . gdkModifierTypeSingleBitList . gdkEventKeyState =<< gdkEventKey e
		GdkEventGdkButtonPress e_ -> True <$ do
			e <- gdkEventButton e_
			putStrLn "BUTTON PRESS"
			print e
			let	axes = gdkEventButtonAxes e
			printDeviceAxes (gdkEventButtonDevice e) axes
			maybe (pure ()) (`printDeviceAxes` axes)
				$ gdkEventButtonSourceDevice e
		GdkEventGdkButtonRelease e -> True <$
			(putStrLn "BUTTON RELEASE" >> (print =<< gdkEventButton e))
		GdkEventGdkDoubleButtonPress e -> True <$
			(putStrLn "DOUBLE BUTTON PRESS" >> (print =<< gdkEventButton e))
		GdkEventGdkTripleButtonPress e -> True <$
			(putStrLn "TRIPLE BUTTON PRESS" >> (print =<< gdkEventButton e))
		GdkEventGdkScroll e -> True <$
			(putStrLn "SCROLL" >> (print =<< gdkEventScroll e))
		GdkEventGdkMotionNotify e_ -> True <$ do
			e <- gdkEventMotion e_
			putStrLn "MOTION NOTIFY"
			print e
			print . gdkModifierTypeSingleBitList
				$ gdkEventMotionState e
			let	axes = gdkEventMotionAxes e
			printDeviceAxes (gdkEventMotionDevice e) axes
			maybe (pure ()) (`printDeviceAxes` axes)
				$ gdkEventMotionSourceDevice e
		GdkEventGdkVisibilityNotify e -> True <$
			(putStrLn "VISIBILITY NOTIFY" >> (print =<< gdkEventVisibility e))
		GdkEventGdkEnterNotify e -> True <$ do
			putStrLn "ENTER NOTIFY"
			print =<< gdkEventCrossing e
		GdkEventGdkLeaveNotify e -> True <$ do
			putStrLn "LEAVE NOTIFY"
			print =<< gdkEventCrossing e
		GdkEventGdkFocusChange e -> True <$ (print =<< gdkEventFocus e)
		GdkEventGdkConfigure e -> True <$ (print =<< gdkEventConfigure e)
		GdkEventGdkPropertyNotify e_ -> True <$ do
			e <- gdkEventProperty e_
			print e
			putStrLn =<< gdkAtomName (gdkEventPropertyAtom e)
		GdkEventGdkWindowState e_ -> True <$ do
			e <- gdkEventWindowState e_
			print e
			print . gdkWindowStateList
				$ gdkEventWindowStateChangedMask e
			print . gdkWindowStateList
				$ gdkEventWindowStateNewWindowState e
		GdkEventGdkAny e -> True <$ (print =<< gdkEventAny e)

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
