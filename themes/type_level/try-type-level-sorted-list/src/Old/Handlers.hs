{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Old.Handlers where

import Control.Monad.State
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.System
import Data.Time.Clock.TAI

import System.Exit

import OpenUnionValue
import Old.React
import Field

handle :: DiffTime -> Field -> EvReqs GuiEv -> StateT AbsoluteTime IO [EvOcc GuiEv]
handle dt f reqs = do
{-
	liftIO do
		putStrLn "requests:"
		print (map prj reqs :: [Maybe MouseMove])
		print (map prj reqs :: [Maybe MouseDown])
		-}
	t1 <- get
	t2 <- liftIO $ systemToTAITime <$> getSystemTime
	let	tdiff = t2 `diffAbsoluteTime` t1
	case time of
		Just t | tdiff >= t -> do
			put $ t `addAbsoluteTime` t1
			pure $ makeTimeObs reqs t
		_ -> do	occs <- withNextEventTimeout' f (round $ dt * 1000000) \case
				Just ButtonEvent {
					ev_event_type = 4,
					ev_x = x, ev_y = y,
					ev_button = b } ->
					pure [inj $ OccurredMouseMove (fromIntegral x, fromIntegral y), inj $ OccurredMouseDown [button b]]
				Just ButtonEvent {
					ev_event_type = 5,
					ev_x = x, ev_y = y,
					ev_button = b } ->
					pure [inj $ OccurredMouseMove (fromIntegral x, fromIntegral y), inj $ OccurredMouseUp [button b]]
--					pure [inj $ OccurredMouseUp [button b]]
				Just MotionEvent { ev_x = x, ev_y = y } ->
					pure [inj $ OccurredMouseMove (fromIntegral x, fromIntegral y)]
				Just ExposeEvent {} -> liftIO (flushField f) >> handle dt f reqs
				Just DestroyWindowEvent {} -> liftIO $ closeField f >> exitSuccess
				Just ev	| isDeleteEvent f ev -> liftIO (destroyField f) >> handle dt f reqs
					| otherwise -> liftIO (print ev) >> handle dt f reqs
				Nothing -> pure []
			put t2
--			pure $ makeTimeObs reqs tdiff ++ occs `filterEvent` reqs
			let	occs' = makeTimeObs reqs tdiff ++ occs `filterEvent` reqs
			{-
			liftIO do
				print (map prj occs' :: [Maybe (Occurred MouseMove)])
				print (map prj occs' :: [Maybe (Occurred MouseDown)])
				-}
			pure occs'
	where time = getWait reqs

handle' :: DiffTime -> Field -> EvReqs GuiEv -> StateT AbsoluteTime IO [EvOcc GuiEv]
handle' dt f reqs = do
	liftIO $ print (map prj reqs :: [Maybe TryWait])
	t1 <- get
	t2 <- liftIO $ systemToTAITime <$> getSystemTime
	let	tdiff = t2 `diffAbsoluteTime` t1
	case time of
		Just t | tdiff >= t -> do
			put $ t `addAbsoluteTime` t1
			pure $ makeTimeObs reqs t
		_ -> do	occs <- withNextEventTimeout f (round $ dt * 1000000) $ pure . mapMaybe eventToEvOcc
			put t2
			pure $ makeTimeObs reqs tdiff ++ occs `filterEvent` reqs
	where time = getWait reqs

eventToEvOcc :: Event -> Maybe (EvOcc GuiEv)
eventToEvOcc = \case
	ButtonEvent {
		ev_event_type = 4,
		ev_button = b } -> Just . inj $ OccurredMouseDown [button b]
	ButtonEvent {
		ev_event_type = 5,
		ev_button = b } -> Just . inj $ OccurredMouseUp [button b]
	_ -> Nothing

button :: Button -> MouseBtn
button = \case
	1 -> MLeft
	2 -> MMiddle
	3 -> MRight
	4 -> MUp
	5 -> MDown
	_ -> error "Unknown button"

getWait :: EvReqs GuiEv -> Maybe DiffTime
getWait reqs = (\(TryWaitReq t) -> t) <$> findValue reqs

findValue :: Member a as => [UnionValue as] -> Maybe a
findValue = listToMaybe . mapMaybe prj

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f = catMaybes . map f

makeTimeObs :: EvReqs GuiEv -> DiffTime -> [EvOcc GuiEv]
makeTimeObs r t = filterMap makeOcc r ++ filterMap makeOcc2 r
	where
	makeOcc u = do
		TryWaitReq _t' <- prj u
		pure . inj $ OccurredTryWait t
	makeOcc2 u = do
		DeltaTimeReq <- prj u
		pure . inj $ OccurredDeltaTime t
