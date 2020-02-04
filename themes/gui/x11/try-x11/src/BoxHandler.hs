{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module BoxHandler (handle) where

import Control.Monad.State
import Data.Maybe
import Data.Set
import Data.Time
import System.Exit

import MonadicFrp
import Field
import ButtonEvent

handle :: Field -> EvReqs GUIEv -> StateT UTCTime IO (EvOccs GUIEv)
handle f r = do
	t <- get
	n <- liftIO getCurrentTime
	put n
	if n `diffUTCTime` t >= fromRational (toRational time) then put (addUTCTime (fromRational $ toRational time) t) >> pure (makeTimeObs r time) else do
		withNextEventTimeout f 50000 \case
			Just (DestroyWindowEvent {}) -> liftIO $ closeField f >> exitSuccess
			Just (ExposeEvent {}) -> liftIO (flushField f) >> handle f r
			Just (ev@ButtonEvent {}) -> case buttonEvent ev of
				Just BtnEvent {	buttonNumber = Button1,
						pressOrRelease = Press,
						position = (x, y) } -> pure $ fromList [
							MouseMove $ Occurred (fromIntegral x, fromIntegral y),
							MouseDown $ Occurred [MLeft]]
				Just BtnEvent {	buttonNumber = Button2,
						pressOrRelease = Press,
						position = (x, y) } -> pure $ fromList [
							MouseMove $ Occurred (fromIntegral x, fromIntegral y),
							MouseDown $ Occurred [MMiddle]]
				Just BtnEvent {	buttonNumber = Button3,
						pressOrRelease = Press,
						position = (x, y) } -> pure $ fromList [
							MouseMove $ Occurred (fromIntegral x, fromIntegral y),
							MouseDown $ Occurred [MRight]]
				Just BtnEvent {	buttonNumber = Button1,
						pressOrRelease = Release,
						position = (x, y) } -> pure $ fromList [
							MouseMove $ Occurred (fromIntegral x, fromIntegral y),
							MouseUp $ Occurred [MLeft]]
				_ -> liftIO (print ev) >> handle f r
			Just (ev@MotionEvent {}) -> case buttonEvent ev of
				Just BtnEvent {	buttonNumber = ButtonX,
						pressOrRelease = Move,
						position = (x, y) } -> do
					pure $ fromList [
						DeltaTime . Occurred . fromRational . toRational $ n `diffUTCTime` t,
						MouseMove $ Occurred (fromIntegral x, fromIntegral y) ]
				_ -> liftIO (print ev) >> handle f r
			Just ev	| isDeleteEvent f ev -> liftIO (destroyField f) >> handle f r
				| otherwise -> liftIO (print ev) >> handle f r
			Nothing -> pure $
				(DeltaTime . Occurred . fromRational . toRational $ n `diffUTCTime` t) `insert`
				makeTimeObs r (fromRational . toRational $ n `diffUTCTime` t)
	where time = getWait r

getWait :: Set GUIEv -> Time
getWait r = if Prelude.null al then inf else minimum al where
	al = filterMap getTime (elems r)
	getTime (TryWait t _) = Just t
	getTime _ = Nothing

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap = (catMaybes .) . Prelude.map

inf :: Time
inf = 1.0 / 0.0

makeTimeObs :: Set GUIEv -> Time -> Set GUIEv
makeTimeObs r t = fromList . filterMap makeOcc $ elems r where
	makeOcc (TryWait t' _) = Just $ TryWait t' (Occurred t)
	makeOcc (DeltaTime _) = Just $ DeltaTime (Occurred t)
	makeOcc _ = Nothing
