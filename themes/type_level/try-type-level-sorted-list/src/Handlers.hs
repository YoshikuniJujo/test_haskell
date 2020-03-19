{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Handlers where

import Control.Monad.State
import Data.Maybe
import Data.List.NonEmpty
import Data.Time
import Data.Time.Clock.System
import Data.Time.Clock.TAI
import System.Exit

import BoxesEvents

import React
import OpenUnionValue
import Sorted
import Field

handleWithoutTime :: Field -> EvReqs GuiEv -> IO (EvOccs GuiEv)
handleWithoutTime f reqs = withNextEvent f \case
	ButtonEvent { ev_event_type = 4, ev_button = b } ->
		pure $ inj (OccurredMouseDown [button b]) :| []
	ButtonEvent { ev_event_type = 5, ev_button = b } ->
		pure $ inj (OccurredMouseUp [button b]) :| []
	_ -> handleWithoutTime f reqs

button :: Button -> MouseBtn
button = \case
	1 -> MLeft; 2 -> MMiddle; 3 -> MRight; 4 -> MUp; 5 -> MDown
	_ -> error "Unknown button"

handle :: DiffTime -> Field -> EvReqs GuiEv -> StateT AbsoluteTime IO (EvOccs GuiEv)
handle dt f reqs = do
--	liftIO . putStrLn $ "handle: " ++ show (prj <$> reqs :: [Maybe TryWait])
	t1 <- get
	t2 <- liftIO $ systemToTAITime <$> getSystemTime
	let	tdiff = t2 `diffAbsoluteTime` t1
	case time of
		Just t | tdiff >= t -> do
			put $ t `addAbsoluteTime` t1
			pure . fromJust $ makeTimeObs reqs t
		_ -> do	occs <- withNextEventTimeout' f (round $ dt * 1000000) \case
				Just ButtonEvent {
					ev_event_type = 4, ev_button = b,
					ev_x = x, ev_y = y } ->
					pure $ [
						inj $ OccurredMouseMove (fromIntegral x, fromIntegral y),
						inj $ OccurredMouseDown [button b] ]
				Just ButtonEvent {
					ev_event_type = 5, ev_button = b,
					ev_x = x, ev_y = y } ->
					pure $ [
						inj $ OccurredMouseMove (fromIntegral x, fromIntegral y),
						inj $ OccurredMouseUp [button b] ]
				Just MotionEvent { ev_x = x, ev_y = y } ->
					pure $ [inj $ OccurredMouseMove (fromIntegral x, fromIntegral y)]
				Just ExposeEvent {} -> liftIO (flushField f) >> pure []
				Just DestroyWindowEvent {} -> liftIO $ closeField f >> exitSuccess
				Just ev	| isDeleteEvent f ev -> liftIO (destroyField f) >> pure []
					| otherwise -> liftIO (print ev) >> pure []
				Nothing -> pure []
			put t2
			let	occs' = maybe [] toList (makeTimeObs reqs tdiff) ++ occs `filterEvent` reqs
--			liftIO . putStrLn $ "here: " ++ show (prj <$> occs' :: [Maybe (Occurred TryWait)])
--			liftIO . putStrLn $ "here: " ++ show (prj <$> occs' :: [Maybe MouseDown])
--			liftIO . putStrLn $ "here: " ++ show (prj <$> occs' :: [Maybe MouseUp])
			case occs' of
				[] -> handle dt f reqs
				_ -> pure $ fromList occs'
	where time = getWait reqs

getWait :: EvReqs GuiEv -> Maybe DiffTime
getWait reqs = (\(TryWaitReq t) -> t) <$> findValue reqs

findValue :: Member a as => [UnionValue as] -> Maybe a
findValue = listToMaybe . mapMaybe prj

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f = catMaybes . (f <$>)

makeTimeObs :: EvReqs GuiEv -> DiffTime -> Maybe (EvOccs GuiEv)
makeTimeObs r t = case filterMap makeOcc r ++ filterMap makeOcc2 r of
	[] -> Nothing
	o : os -> Just $ o :| os
	where
	makeOcc u = do
		TryWaitReq _t' <- prj u
		pure . inj $ OccurredTryWait t
	makeOcc2 u = do
		DeltaTimeReq <- prj u
		pure . inj $ OccurredDeltaTime t

filterEvent :: [UnionValue (Map Occurred es)] -> EvReqs es -> [UnionValue (Map Occurred es)]
filterEvent = intersection' @Occurred
