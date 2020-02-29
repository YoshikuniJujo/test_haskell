{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Field
import ButtonEvent

checkMouseMove :: Pixel -> IO ()
checkMouseMove c = do
	f <- openField "ゲーム" [pointerMotionMask]
	loop f
	where
	loop f = do
		withNextEvent f \ev -> case buttonEvent ev of
			Just BtnEvent { position = (x, y) } -> do
				clearField f
				fillRect f c (fromIntegral x) (fromIntegral y) 50 50
				flushField f
			Nothing -> pure ()
		loop f
