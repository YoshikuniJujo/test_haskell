{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Prelude hiding (repeat)

import Control.Monad.State
import Data.Set
import Data.Time
import System.Exit

import MonadicFrp
import Field
import ButtonEvent

main :: IO ()
main = do
	f <- openField "ダブルクリック" [exposureMask, buttonPressMask, pointerMotionMask]
	ct <- getCurrentTime
	_ <- (`runStateT` ct)
		$ interpretSig (handle f) (liftIO . print) . repeat . drClickOn $ Rect (10, 10) (500, 400)
--		$ interpretSig (handle f) (liftIO . print) . drClickOn $ Rect (10, 10) (500, 400)
	closeField f

handle :: Field -> EvReqs GUIEv -> StateT UTCTime IO (EvOccs GUIEv)
handle f r = do
	t <- get
	n <- liftIO getCurrentTime
	put n
	withNextEventTimeout f 50000 \case
		Just (DestroyWindowEvent {}) -> liftIO $ closeField f >> exitSuccess
		Just (ExposeEvent {}) -> liftIO (flushField f) >> handle f r
		Just (ev@ButtonEvent {}) -> case buttonEvent ev of
			Just BtnEvent {	buttonNumber = Button3,
					pressOrRelease = Press } -> pure . singleton . MouseDown $ Occurred [MRight]
			_ -> liftIO (print ev) >> handle f r
		Just (ev@MotionEvent {}) -> case buttonEvent ev of
			Just BtnEvent {	buttonNumber = ButtonX,
					pressOrRelease = Move,
					position = (x, y) } -> do
				pure . singleton . MouseMove $ Occurred (fromIntegral x, fromIntegral y)
			_ -> liftIO (print ev) >> handle f r
		Just ev	| isDeleteEvent f ev -> liftIO (destroyField f) >> handle f r
			| otherwise -> liftIO (print ev) >> handle f r
		Nothing -> pure . singleton . DeltaTime . Occurred . fromRational . toRational $ (n `diffUTCTime` t)
