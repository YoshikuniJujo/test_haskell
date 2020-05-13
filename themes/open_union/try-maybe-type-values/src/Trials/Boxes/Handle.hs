{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Boxes.Handle (SigG, handleWithoutTime, handle, AB(..)) where

import Foreign.C.Types (CInt)
import Control.Arrow
import Control.Monad.State (StateT, get, put, lift)
import Data.Type.Set (Set(Nil), Singleton, (:-))
import Data.UnionSet (prj, singleton, (>-), expand, collapse, merge')
import Data.Time (DiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (AbsoluteTime, diffAbsoluteTime, addAbsoluteTime)
import System.Exit (exitSuccess)

import Trials.Boxes.Events (
	GuiEv, SigG,
	MouseDown, MouseUp, MouseMove, TryWait(..), DeltaTime(..), Occurred(..),
	MouseBtn(..) )
import MonadicFrp.Handle
import Field (
	Field, Event(..), Button,
	withNextEvent, withNextEventTimeout',
	flushField, isDeleteEvent, destroyField, closeField )

handleWithoutTime :: Field -> EvReqs GuiEv -> IO (EvOccs GuiEv)
handleWithoutTime f reqs = withNextEvent f \case
	ButtonEvent { ev_event_type = 4, ev_button = eb }
		| Just b <- button eb ->
			pure . expand . singleton $ OccMouseDown [b]
	_ -> handleWithoutTime f reqs

data AB = A | B AbsoluteTime deriving Show

handle :: DiffTime -> Field -> HandleSt AB (StateT AbsoluteTime IO) GuiEv
handle prd f = retrySt \case A -> handleA prd f; B now -> handleB now

handleA :: DiffTime -> Field -> EvReqs GuiEv -> StateT AbsoluteTime IO (Maybe (EvOccs GuiEv), AB)
handleA prd f reqs = do
	r1 <- maybe (pure Nothing) (lift . handleMouse prd f) $ collapse reqs
	now <- lift $ systemToTAITime <$> getSystemTime
	maybe	((expand <$> r1, A) <$ put now)
		(((((`merge'` r1) . Just) `first`) <$>) . handleTime now) $ collapse reqs

handleB :: AbsoluteTime -> EvReqs GuiEv -> StateT AbsoluteTime IO (Maybe (EvOccs GuiEv), AB)
handleB now reqs = maybe
	((Nothing, A) <$ put now)
	((((Just . expand) `first`) <$>) . handleTime now) $ collapse reqs

handleTime :: Monad m => AbsoluteTime ->
	EvReqs (TryWait :- DeltaTime :- 'Nil) ->
	StateT AbsoluteTime m (EvOccs (TryWait :- DeltaTime :- 'Nil), AB)
handleTime now reqs = do
	lst <- get
	let	dt = now `diffAbsoluteTime` lst
	case prj reqs of
		Just (TryWaitReq t)
			| t < dt -> do
				put $ t `addAbsoluteTime` lst
				pure (OccTryWait t >- singleton (OccDeltaTime t), B now)
			| otherwise -> do
				put now
				pure (OccTryWait dt >- singleton (OccDeltaTime dt), A)
		Nothing -> do
			put now
			pure (expand . singleton $ OccDeltaTime dt, A)

handleMouse :: DiffTime -> Field -> Handle' IO (MouseDown :- MouseUp :- MouseMove :- 'Nil)
handleMouse prd f _reqs = withNextEventTimeout'' f prd \case
	Just ButtonEvent {
		ev_event_type = 4, ev_button = eb,
		ev_x = x, ev_y = y }
		| Just b <- button eb -> pure . Just
			. expand $ mouseDownOcc x y [b]
	Just ButtonEvent {
		ev_event_type = 5, ev_button = eb,
		ev_x = x, ev_y = y }
		| Just b <- button eb -> pure . Just
			. expand $ mouseUpOcc x y [b]
	Just MotionEvent { ev_x = x, ev_y = y } ->
		pure . Just . expand $ mouseMoveOcc x y
	Just ExposeEvent {} ->
		Nothing <$ flushField f
	Just DestroyWindowEvent {} ->
		closeField f >> exitSuccess
	Just ev	| isDeleteEvent f ev ->
		Nothing <$ destroyField f
	_ -> pure Nothing

mouseDownOcc ::
	CInt -> CInt -> [MouseBtn] -> EvOccs (MouseMove :- MouseDown :- 'Nil)
mouseDownOcc x y bs =OccMouseDown bs >- mouseMoveOcc x y

mouseUpOcc ::
	CInt -> CInt -> [MouseBtn] -> EvOccs (MouseMove :- MouseUp :- 'Nil)
mouseUpOcc x y bs = OccMouseUp bs >- mouseMoveOcc x y

mouseMoveOcc :: CInt -> CInt -> EvOccs (Singleton MouseMove)
mouseMoveOcc x y = singleton $ OccMouseMove (fromIntegral x, fromIntegral y)

withNextEventTimeout'' :: Field -> DiffTime -> (Maybe Event -> IO a) ->  IO a
withNextEventTimeout'' f = withNextEventTimeout' f . round . (* 1000000)

button :: Button -> Maybe MouseBtn
button = \case
	1 -> Just MLeft; 2 -> Just MMiddle; 3 -> Just MRight
	4 -> Just MUp; 5 -> Just MDown; _ -> Nothing
