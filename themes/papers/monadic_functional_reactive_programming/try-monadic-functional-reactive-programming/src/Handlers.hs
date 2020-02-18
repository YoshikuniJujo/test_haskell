{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Handlers where

import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Set
import Data.Time
import System.Exit

import GuiEv
import React

import Field
import ButtonEvent
import FieldAndMonadicFrp

handleMotion :: Field -> EvReqs GuiEv -> IO (EvOccs GuiEv)
handleMotion f r = withNextEvent f \case
	DestroyWindowEvent {} -> closeField f >> exitSuccess
	ev@ButtonEvent {} -> case buttonEvent ev of
		Just BtnEvent {
			buttonNumber = bn,
			pressOrRelease = Press,
			position = (x, y) } -> do
			maybe (handleMotion f r)
				(pure . fromList . (MouseMove (Occurred (x, y)) :) . (: []) . MouseDown
					. Occurred . (: [])) $ mouseButton bn
		Just BtnEvent {
			buttonNumber = bn,
			pressOrRelease = Release,
			position = (x, y) } -> do
			maybe (handleMotion f r)
				(pure . fromList . (MouseMove (Occurred (x, y)) :) . (: []) . MouseUp
					. Occurred . (: [])) $ mouseButton bn
		_ -> handleMotion f r
	ev@MotionEvent {} -> case buttonEvent ev of
		Just BtnEvent {
			buttonNumber = ButtonX,
			pressOrRelease = Move,
			position = (x, y) } -> do
			pure . singleton . MouseMove $ Occurred (x, y)
		_ -> handleMotion f r
	ExposeEvent {} -> flushField f >> handleMotion f r
	e	| isDeleteEvent f e -> destroyField f >> handleMotion f r
		| otherwise -> print e >> handleMotion f r

handleDelta :: NominalDiffTime -> Field -> EvReqs GuiEv -> StateT UTCTime IO (EvOccs GuiEv)
handleDelta dt f r = do
	t <- get
	n <- liftIO getCurrentTime
	put n
	if n `diffUTCTime` t >= time
	then do	put $ addUTCTime time t
		pure $ makeTimeObs r time
	else withNextEventTimeout f (round $ dt * 1000000) \case
		[] -> do
			pure . makeTimeObs r $ n `diffUTCTime` t
		es	| Just _ <- find isExposeEvent es -> liftIO (flushField f) >> handleDelta dt f r
			| Just _ <- find isDestroyWindowEvent es ->
				liftIO $ putStrLn ("destroy: " ++ show es) >> closeField f >> exitSuccess
			| Just _ <- find (isDeleteEvent f) es -> liftIO (print es >> destroyField f) >> handleDelta dt f r
			| otherwise -> liftIO (putStrLn $ "event occur: " ++ show es) >> handleDelta dt f r
	where time = getWait r

isDestroyWindowEvent :: Field.Event -> Bool
isDestroyWindowEvent DestroyWindowEvent {} = True
isDestroyWindowEvent _ = False

isExposeEvent :: Field.Event -> Bool
isExposeEvent ExposeEvent {} = True
isExposeEvent _ = False

getWait :: Set GuiEv -> Time
getWait r = if Prelude.null al then big else minimum al where
	al = filterMap getTime (elems r)
	getTime (TryWait t _) = Just t
	getTime _ = Nothing

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap = (catMaybes .) . Prelude.map

big :: Time
big = 2 ^ (2 ^ (5 :: Int) :: Int)

makeTimeObs :: Set GuiEv -> Time -> Set GuiEv
makeTimeObs r t = fromList . filterMap makeOcc $ elems r where
	makeOcc (TryWait t' _) = Just $ TryWait t' (Occurred t)
	makeOcc (DeltaTime _) = Just $ DeltaTime (Occurred t)
	makeOcc _ = Nothing
