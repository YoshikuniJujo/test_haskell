{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Boxes.Handlers where

import Control.Monad.State
import Data.Maybe
import Data.Time
import Data.Time.Clock.System
import Data.Time.Clock.TAI
import System.Exit

import Boxes.Events
import MonadicFrp.React
import Data.UnionList
import Data.Sorted
import Field

handleWithoutTime :: Field -> EvReqs GuiEv -> IO (EvOccs GuiEv)
handleWithoutTime f reqs = withNextEvent f \case
	ButtonEvent { ev_event_type = 4, ev_button = eb } | Just b <- button eb ->
		pure . expand $ OccurredMouseDown [b] >+ UnionListNil
	_ -> handleWithoutTime f reqs

button :: Button -> Maybe MouseBtn
button = \case
	1 -> Just MLeft; 2 -> Just MMiddle; 3 -> Just MRight
	4 -> Just MUp; 5 -> Just MDown; _ -> Nothing

handle :: DiffTime -> Field -> EvReqs GuiEv -> StateT AbsoluteTime IO (EvOccs GuiEv)
handle dt f reqs = do
	t1 <- get
	t2 <- liftIO $ systemToTAITime <$> getSystemTime
	let	tdiff = t2 `diffAbsoluteTime` t1
	case getWait reqs of
		Just t | tdiff >= t -> do
			put $ t `addAbsoluteTime` t1
			pure . expand . fromJust $ makeTimeObs reqs t
		_ -> do
			put t2
			(moccs :: Maybe (EvOccs GuiEv))  <- withNextEventTimeout' f (round $ dt * 1000000) \case
				Just ButtonEvent {
					ev_event_type = 4, ev_button = eb,
					ev_x = x, ev_y = y } | Just b <- button eb ->
					pure . Just $ expand (
						OccurredMouseMove (fromIntegral x, fromIntegral y) >+.
						(OccurredMouseDown [b] >+ UnionListNil)
							:: UnionList 'True (Occurred :$: (MouseMove :- Singleton MouseDown)))
				Just ButtonEvent {
					ev_event_type = 5, ev_button = eb,
					ev_x = x, ev_y = y } | Just b <- button eb ->
					pure . Just $ expand (
						OccurredMouseMove (fromIntegral x, fromIntegral y) >+.
						(OccurredMouseUp [b] >+ UnionListNil)
							:: UnionList 'True (Occurred :$: (MouseMove :- Singleton MouseUp)))
				Just MotionEvent { ev_x = x, ev_y = y } ->
					pure . Just . expand $
						OccurredMouseMove (fromIntegral x, fromIntegral y) >+ UnionListNil
				Just ExposeEvent {} -> liftIO (flushField f) >> pure Nothing
				Just DestroyWindowEvent {} -> liftIO $ closeField f >> exitSuccess
				Just ev	| isDeleteEvent f ev -> liftIO (destroyField f) >> pure Nothing
					| otherwise -> pure Nothing
				_ -> pure Nothing
			let	mtoccs = makeTimeObs reqs tdiff
			case (moccs, mtoccs) of
				(Just occs, Just toccs) -> pure $ toccs `merge_` occs
				(Just occs, Nothing) -> pure $ expand occs
				(Nothing, Just toccs) -> pure $ expand toccs
				(Nothing, Nothing) -> handle dt f reqs

getWait :: EvReqs GuiEv -> Maybe DiffTime
getWait reqs = (\(TryWaitReq t) -> t) <$> prj reqs

makeTimeObs :: EvReqs GuiEv -> DiffTime -> Maybe (UnionList 'True (Occurred :$: TryWait :- DeltaTime :- 'Nil))
makeTimeObs r t = case (prj r, prj r) of
	(Just (TryWaitReq _t'), Just DeltaTimeReq) ->
		Just $ OccurredDeltaTime t >+. (OccurredTryWait t >+ UnionListNil)
	(Just (TryWaitReq _t'), Nothing) ->
		Just . expand $ OccurredTryWait t >+ UnionListNil
	(Nothing, Just DeltaTimeReq) -> Just . expand $ OccurredDeltaTime t >+ UnionListNil
	(Nothing, Nothing) -> Nothing
