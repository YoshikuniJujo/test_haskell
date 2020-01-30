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
	phs <- atomically $ newTVar 0
	cl1 <- atomically $ newTVar False
	scl <- atomically $ newTVar False
	pos <- atomically $ newTVar (0, 0)
	f <- openField "長方形" [
		exposureMask,
		buttonPressMask, buttonReleaseMask, button1MotionMask ]
	void . forkIO $ loop do
		(p, rs) <- atomically $ do
			check =<< readTVar fl <* writeTVar fl False
			(,) <$> readTVar phs <*> readTVar rcts
		clearField f
		drawRects f p rs
		flushField f
	void . forkIO $ loop do
		threadDelay 10000
		atomically $ do
			modifyTVar phs (+ 1 / 12)
			writeTVar fl True
	void . forkIO $ loop do
		atomically $ check =<< readTVar cl1
		threadDelay 500000
		atomically do
			writeTVar scl =<< readTVar cl1
			writeTVar cl1 False
	void . forkIO . loop $ atomically do
		check =<< readTVar scl
		p <- readTVar pos
		modifyTVar rcts $ sink1 p
		writeTVar scl False
	while $ withNextEvent f \case
		DestroyWindowEvent {} -> False <$ closeField f
		ExposeEvent {} -> True <$ flushField f
		ev@ButtonEvent {} -> True <$ case buttonEvent ev of
			Just BtnEvent {	buttonNumber = Button1,
					pressOrRelease = Press,
					position = (x, y) } -> atomically do
				writeTVar scl =<< readTVar cl1
				writeTVar cl1 False
				modifyTVar rcts $ pushRectangle (fromIntegral x, fromIntegral y)
			Just BtnEvent {	buttonNumber = Button1,
					pressOrRelease = Release,
					position = (x, y) } -> atomically do
				writeTVar scl =<< readTVar cl1
				writeTVar cl1 False
				modifyTVar rcts $ dragRectangle (fromIntegral x, fromIntegral y)
				modifyTVar rcts floatHead
				writeTVar fl True
			Just BtnEvent {	buttonNumber = Button2,
					pressOrRelease = Press,
					position = (x, y) } -> atomically do
				writeTVar scl =<< readTVar cl1
				writeTVar cl1 False
				modifyTVar rcts $ rotColor (fromIntegral x, fromIntegral y)
				writeTVar fl True
			Just BtnEvent {	buttonNumber = Button3,
					pressOrRelease = Press,
					position = (x, y) } -> atomically do
				c1 <- readTVar cl1
				if c1
				then do	modifyTVar rcts $ remove1 (fromIntegral x, fromIntegral y)
					writeTVar cl1 False
				else do	-- modifyTVar rcts $ sink1 (fromIntegral x, fromIntegral y)
					writeTVar pos (fromIntegral x, fromIntegral y)
					writeTVar cl1 True
				writeTVar fl True
			Just e@BtnEvent { pressOrRelease = Press } -> do
				atomically do
					writeTVar scl =<< readTVar cl1
					writeTVar cl1 False
				print e
			e -> print e
		ev@MotionEvent {} -> True <$ case buttonEvent ev of
			Just BtnEvent {	buttonNumber = ButtonX,
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

drawRects :: Field -> Double -> [Rectangle] -> IO ()
drawRects f p rs = drawRect f p `mapM_` reverse rs

drawRect :: Field -> Double -> Rectangle -> IO ()
drawRect f p r = fillRect f (color c) l u w h
	where
	((l_, u_, w_, h_), c) = calcRectangle 10 p r
	(l, u) = (fromIntegral l_, fromIntegral u_)
	(w, h) = (fromIntegral w_, fromIntegral h_)

color :: Color -> Pixel
color Red = 0xff0000
color Green = 0x00ff00
color Blue = 0x0000ff
color Yellow = 0xffff00
color Cyan = 0x00ffff
color Magenta = 0xff00ff
