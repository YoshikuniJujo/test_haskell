{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryReact where

import Control.Monad.State
-- import Data.Time.Clock.TAI
import Data.Time.Clock.System

import Sorted
import OpenUnionValue
import React
import Field

import Handlers

tryMouseDown :: IO ()
tryMouseDown = do
	f <- openField "tryMouseDown" [exposureMask, buttonPressMask]
	interpret (handleMouseDown f) mouseDown >>= print
	closeField f

handleMouseDown :: Field -> EvReqs (Singleton MouseDown) -> IO [EvOcc (Singleton MouseDown)]
handleMouseDown f reqs = withNextEvent f \case
	ExposeEvent {} -> flushField f >> handleMouseDown f reqs
	ev@ButtonEvent {
		ev_event_type = 4,
		ev_button = b
		} -> print ev >> print b >> pure [inj $ OccurredMouseDown [button b]]
	ev -> print ev >> handleMouseDown f reqs

handleMouseDownUp :: Field -> EvReqs (Insert MouseUp (Singleton MouseDown)) -> IO [EvOcc (Insert MouseUp (Singleton MouseDown))]
handleMouseDownUp f reqs = withNextEvent f \case
	ExposeEvent {} -> flushField f >> handleMouseDownUp f reqs
	ev@ButtonEvent {
		ev_event_type = 4,
		ev_button = b
		} -> print ev >> print b >> pure [inj $ OccurredMouseDown [button b]]
	ev@ButtonEvent {
		ev_event_type = 5,
		ev_button = b
		} -> print ev >> print b >> pure [inj $ OccurredMouseUp [button b]]
	ev -> print ev >> handleMouseDownUp f reqs

trySameClick :: IO ()
trySameClick = do
	f <- openField "trySameClick" [exposureMask, buttonPressMask]
	interpret (handleMouseDown f) sameClick >>= print
	closeField f

{-
button :: Button -> MouseBtn
button 1 = MLeft
button 2 = MMiddle
button 3 = MRight
button 4 = MUp
button 5 = MDown
button _ = error "Unknown button"
-}

tryLeftClick :: IO ()
tryLeftClick = do
	f <- openField "tryLeftClick" [exposureMask, buttonPressMask]
	interpret (handleMouseDown f) leftClick
	closeField f

tryBefore :: IO ()
tryBefore = do
	f <- openField "tryBefore" [exposureMask, buttonPressMask]
	interpret (handleMouseDown f) (leftClick `before` rightClick) >>= print
	closeField f

tryBefore2 :: IO ()
tryBefore2 = do
	f <- openField "tryBefore" [exposureMask, buttonPressMask, buttonReleaseMask]
	interpret (handleMouseDownUp f) (leftRelease `before` rightClick) >>= print
	closeField f

tryDoubler :: IO ()
tryDoubler = do
	f <- openField "tryDoubler" [exposureMask, buttonPressMask, buttonReleaseMask]
	now <- systemToTAITime <$> getSystemTime
	interpret (handle 0.1 f) doubler `runStateT` now >>= print
	closeField f
