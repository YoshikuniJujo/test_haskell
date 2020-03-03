{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Control.Arrow
import Data.Int

import Field
import ButtonEvent

checkMouseMove :: IO ()
checkMouseMove = do
	f <- openField "ゲーム" [pointerMotionMask]
	loop f
	where
	loop f = do
		withNextEvent f \ev -> case buttonEvent ev of
			Just BtnEvent { position = (x, y) } -> do
				clearField f
				fillPolygon f 0x00ff00 . turtle 3 $ Point (fromIntegral x) (fromIntegral y)
				flushField f
			Nothing -> pure ()
		loop f

turtle :: Int32 -> Point -> [Point]
turtle sz (Point x y) = uncurry Point . ((x +) *** (y +)) . ((* sz) *** (* sz)) <$> halfTurtle ++ reverseTurtle halfTurtle

halfTurtle :: [(Int32, Int32)]
halfTurtle = [
	(- 10, 0), (- 8, 3), (- 10, 5), (- 7, 9), (- 5, 6), (0, 8),
	(4, 7), (6, 10), (8, 7), (7, 5), (10, 2), (13, 3), (16, 0) ]

reverseTurtle :: [(Int32, Int32)] -> [(Int32, Int32)]
reverseTurtle = reverse . ((id *** negate) <$>)
