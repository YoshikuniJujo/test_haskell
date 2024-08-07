{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent
import Data.KeySym
import System.Environment
import System.Console.GetOpt

import Graphics.Gdk.GdkDisplay
import Graphics.Gdk.Windows
import Graphics.Gdk.Windows.GdkWindowAttr
import Graphics.Gdk.Windows.GdkEventMask
import Graphics.Gdk.Events
import Graphics.Gdk.EventStructures

main :: IO ()
main = do
	(ss, _as, es) <- getOpt Permute [optShowEvents] <$> getArgs
	putStrLn `mapM_` es
	_ <- gdkDisplayOpen ""
	win <- gdkToplevelNew Nothing $ minimalGdkWindowAttr
		(gdkEventMaskMultiBits [GdkKeyPressMask]) 700 500
	gdkWindowShow win
	gdkSetShowEvents $ optsToShowEvents ss
	print =<< gdkGetShowEvents
	mainLoop 100000 \e -> do
		print =<< gdkEventsPending
		processEvent e

processEvent :: GdkEvent s -> IO Bool
processEvent = \case
	GdkEventGdkDelete e -> False <$ (print =<< gdkEventAny e)
	GdkEventGdkWindowState e_ -> True <$ do
		e <- gdkEventWindowState e_
		print e
		print . gdkWindowStateList
			$ gdkEventWindowStateChangedMask e
		print . gdkWindowStateList
			$ gdkEventWindowStateNewWindowState e
	GdkEventGdkConfigure e -> True <$ (print =<< gdkEventConfigure e)
	GdkEventGdkMap e -> True <$ (print =<< gdkEventAny e)
	GdkEventGdkVisibilityNotify e -> True <$ (print =<< gdkEventVisibility e)
	GdkEventGdkKeyPress e_ -> do
		e <- gdkEventKey e_
		print e
		pure case gdkEventKeyKeyval e of Xk_q -> False; _ -> True
	GdkEventGdkAny e -> True <$ (print =<< gdkEventAny e)

mainLoop :: Int -> (forall s . GdkEvent s -> IO Bool) -> IO ()
mainLoop slp f = gdkWithEvent \case
	Nothing -> threadDelay slp >> mainLoop slp f
	Just e -> f e >>= \case False -> pure (); True -> mainLoop slp f

gdkWithAllEvents :: (forall s . GdkEvent s -> IO a) -> IO [a]
gdkWithAllEvents f = gdkWithEvent \case
	Nothing -> pure []
	Just e -> (:) <$> f e <*> gdkWithAllEvents f

gdkWithAllEvents_ :: (forall s . GdkEvent s -> IO a) -> IO ()
gdkWithAllEvents_ f = gdkWithEvent \case
	Nothing -> pure ()
	Just e -> f e >> gdkWithAllEvents_ f

data OptSetting = OptShowEvents deriving Show

optShowEvents :: OptDescr OptSetting
optShowEvents = Option ['s'] ["show-events"] (NoArg OptShowEvents) "Show events"

optsToShowEvents :: [OptSetting] -> Bool
optsToShowEvents [] = False
optsToShowEvents (OptShowEvents : _) = True
