{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Control.Arrow ((***), first, second)
import Data.IORef
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
				fillPolygon f 0x00ff00 . moveTo (Point (fromIntegral x) (fromIntegral y)) $ turtle 3
				flushField f
			Nothing -> pure ()
		loop f

checkKey :: IO ()
checkKey = do
	pos <- newIORef (100, 100)
	dir <- newIORef DRight
	f <- openField "ゲーム" [keyPressMask]
	loop f dir pos
	where
	loop f dir pos = do
		withNextKeyEvent f \case
			(_, "[") -> modifyIORef pos (second (subtract 5)) >> writeIORef dir DUp
			(_, "/") -> modifyIORef pos (second (+ 5)) >> writeIORef dir DDown
			(_, ";") -> modifyIORef pos (first (subtract 5)) >> writeIORef dir DLeft
			(_, "'") -> modifyIORef pos (first (+ 5)) >> writeIORef dir DRight
			str -> print str
		(x, y) <- readIORef pos
		clearField f
		d <- readIORef dir
		fillPolygon f 0x00ff00 . moveTo (Point x y) $ iterate (turnLeft <$>) (turtle 3) !! fromEnum d
		flushField f
		loop f dir pos

data Direction = DRight | DDown | DLeft | DUp deriving (Show, Enum)

turtle :: Int32 -> [Point]
turtle sz = uncurry Point . ((* sz) *** (* sz)) <$> halfTurtle ++ reverseTurtle halfTurtle

moveTo :: Point -> [Point] -> [Point]
moveTo (Point x y) = (uncurry Point . ((x +) *** (y +)) . (\(Point x' y') -> (x', y')) <$>)

turnLeft :: Point -> Point
turnLeft (Point x y) = Point (- y) x

halfTurtle :: [(Int32, Int32)]
halfTurtle = [
	(- 10, 0), (- 8, 3), (- 10, 5), (- 7, 9), (- 5, 6), (0, 8),
	(4, 7), (6, 10), (8, 7), (7, 5), (10, 2), (13, 3), (16, 0) ]

reverseTurtle :: [(Int32, Int32)] -> [(Int32, Int32)]
reverseTurtle = reverse . ((id *** negate) <$>)
