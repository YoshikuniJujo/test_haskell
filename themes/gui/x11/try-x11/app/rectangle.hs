{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import Field
import ButtonEvent
import Rectangle

main :: IO ()
main = do
	fl <- atomically $ newTVar False
	rcts <- atomically $ newTVar []
	f <- openField "長方形" [
		exposureMask,
		buttonPressMask, buttonReleaseMask, button1MotionMask ]
	void . forkIO $ loop do
		atomically $ check =<< readTVar fl <* writeTVar fl False
		clearField f
		(>>) <$> drawRects f <*> print =<< atomically (readTVar rcts)
		flushField f
	while $ withNextEvent f \case
		DestroyWindowEvent {} -> False <$ closeField f
		ExposeEvent {} -> True <$ flushField f
		ev@ButtonEvent {} -> True <$ case buttonEvent ev of
			Just BtnEvent {
				buttonNumber = Button1,
				pressOrRelease = Press,
				position = (x, y) } -> atomically do
				modifyTVar rcts $ pushRectangle (fromIntegral x, fromIntegral y)
			e -> print e
		ev@MotionEvent {} -> True <$ case buttonEvent ev of
			Just BtnEvent {
				buttonNumber = ButtonX,
				pressOrRelease = Move,
				position = (x, y) } -> atomically do
				modifyTVar rcts $ dragRectangle (fromIntegral x, fromIntegral y)
				writeTVar fl True
			e -> print e
		ev	| isDeleteEvent f ev -> True <$ destroyField f
			| otherwise -> print ev >> pure True

while :: Monad m => m Bool -> m ()
while act = (`when` while act) =<< act

loop :: Monad m => m a -> m ()
loop act = act >> loop act

drawRects :: Field -> [Rectangle] -> IO ()
drawRects f rs = drawRect f `mapM_` reverse rs

drawRect :: Field -> Rectangle -> IO ()
drawRect f r = fillRect f (color c) l u w h
	where
	((l_, u_, w_, h_), c) = calcRectangle 0 0 r
	(l, u) = (fromIntegral l_, fromIntegral u_)
	(w, h) = (fromIntegral w_, fromIntegral h_)

color :: Color -> Pixel
color Red = 0xff0000
color Green = 0x00ff00
color Blue = 0x0000ff
color Yellow = 0xffff00
color Cyan = 0x00ffff
color Magenta = 0xff00ff
