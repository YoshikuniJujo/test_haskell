{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryMyInterface.Boxes.Handlers (handleWithoutTime, handle) where

import Foreign.C.Types (CInt)
import Control.Monad (void)
import Control.Monad.State (StateT, get, put, liftIO)
import Data.Type.Set (Set(Nil), Singleton, (:-))
import Data.Maybe (fromJust)
import Data.Time (DiffTime)
import Data.Time.Clock.System (getSystemTime, systemToTAITime)
import Data.Time.Clock.TAI (AbsoluteTime, diffAbsoluteTime, addAbsoluteTime)
import System.Exit (exitSuccess)

import TryMyInterface.Boxes.Events (
	GuiEv,
	MouseDown, MouseUp, MouseMove, TryWait(..), DeltaTime(..), Occurred(..),
	MouseBtn(..) )
import MonadicFrp.MyInterface (
	EvReqs, EvOccs,
	(>-), singleton, expand, merge', prj )
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

handle :: DiffTime ->
	Field -> EvReqs GuiEv -> StateT AbsoluteTime IO (EvOccs GuiEv)
handle prd f reqs = do
	(lst, now) <- (,) <$> get <*> liftIO (systemToTAITime <$> getSystemTime)
	let	dt = now `diffAbsoluteTime` lst
	case getTryWaitReq <$> prj reqs of
		Just t | dt >= t -> do
			put $ t `addAbsoluteTime` lst
			pure . expand . fromJust $ mkTimeObs reqs t
		_ -> do	put now
			moccs <- liftIO $ withNextEventTimeout'' f prd \case
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
			void $ pure (moccs :: Maybe (EvOccs GuiEv))
			maybe (handle dt f reqs) pure
				$ moccs `merge'` mkTimeObs reqs dt

mouseDownOcc ::
	CInt -> CInt -> [MouseBtn] -> EvOccs (MouseMove :- MouseDown :- 'Nil)
mouseDownOcc x y bs =OccMouseDown bs >- mouseMoveOcc x y

mouseUpOcc ::
	CInt -> CInt -> [MouseBtn] -> EvOccs (MouseMove :- MouseUp :- 'Nil)
mouseUpOcc x y bs = OccMouseUp bs >- mouseMoveOcc x y

mouseMoveOcc :: CInt -> CInt -> EvOccs (Singleton MouseMove)
mouseMoveOcc x y = singleton $ OccMouseMove (fromIntegral x, fromIntegral y)

mkTimeObs :: EvReqs GuiEv ->
	DiffTime -> Maybe (EvOccs (TryWait :- DeltaTime :- 'Nil))
mkTimeObs r t = mkTryWait r t `merge'` mkDeltaTime r t

mkTryWait :: EvReqs GuiEv -> DiffTime -> Maybe (EvOccs (Singleton TryWait))
mkTryWait r t = (<$> prj r) \(TryWaitReq _) -> singleton $ OccTryWait t

mkDeltaTime :: EvReqs GuiEv -> DiffTime -> Maybe (EvOccs (Singleton DeltaTime))
mkDeltaTime r t = (<$> prj r) \DeltaTimeReq -> singleton $ OccDeltaTime t

withNextEventTimeout'' :: Field -> DiffTime -> (Maybe Event -> IO a) ->  IO a
withNextEventTimeout'' f = withNextEventTimeout' f . round . (* 1000000)

button :: Button -> Maybe MouseBtn
button = \case
	1 -> Just MLeft; 2 -> Just MMiddle; 3 -> Just MRight
	4 -> Just MUp; 5 -> Just MDown; _ -> Nothing
