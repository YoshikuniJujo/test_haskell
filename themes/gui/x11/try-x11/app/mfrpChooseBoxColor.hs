{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.State
import Data.Set
import Data.Time
import System.Exit

import MonadicFrp
import Field
import ButtonEvent
import ColorToPixel

main :: IO ()
main = do
	f <- openField "ふるえて色が変わる長方形" [exposureMask, buttonPressMask]
	ct <- getCurrentTime
	_ <- (`runStateT` ct)
		$ interpretSig (handle f) (liftIO . fillBox f) . chooseBoxColor $ Rect (100, 50) (300, 200)
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
			Just BtnEvent {	buttonNumber = Button2,
					pressOrRelease = Press } -> pure . singleton . MouseDown $ Occurred [MMiddle]
			Just BtnEvent {	buttonNumber = Button3,
					pressOrRelease = Press } -> pure . singleton . MouseDown $ Occurred [MRight]
			_ -> handle f r
		Just ev	| isDeleteEvent f ev -> liftIO (destroyField f) >> handle f r
			| otherwise -> liftIO (print ev) >> handle f r
		Nothing -> pure . singleton . DeltaTime . Occurred . fromRational . toRational $ (n `diffUTCTime` t)

fillBox :: Field -> Box -> IO ()
fillBox f (Box (Rect (l, t) (r, u)) c) = do
	clearField f
	fillRect f (color c) (round l) (round t) (round $ r - l) (round $ u - t)
	flushField f
