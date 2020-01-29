{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import Field
import ButtonEvent

colors :: [Pixel]
colors = [0xff0000, 0x00ff00, 0x0000ff, 0xffff00, 0x00ffff, 0xff00ff] ++ colors

main :: IO ()
main = do
	(ul, br, vb, vbon, clr, rcts, fl) <- atomically $ (,,,,,,)
		<$> newTVar (0, 0) <*> newTVar (0, 0)
		<*> newTVar 0 <*> newTVar False <*> newTVar 0xff0000
		<*> newTVar []
		<*> newTVar False
	f <- openField "長方形" [
		exposureMask, buttonPressMask, buttonReleaseMask, button1MotionMask ]
	void . forkIO $ loop do
		((ulx_, uly_), (brx_, bry_), v, c, rs) <- atomically $ do
			check =<< readTVar fl <* writeTVar fl False
			(,,,,) <$> readTVar ul <*> readTVar br <*> readTVar vb <*> readTVar clr <*> readTVar rcts
		let	(ulx, uly, w, h) = calcRect ulx_ uly_ brx_ bry_ v
		clearField f
		(\((x, y, w', h'), c') -> fillRect f (colors !! c') x y w' h') `mapM_` reverse rs
		fillRect f (colors !! c) ulx uly w h
		flushField f
	void . forkIO $ loop do
		atomically $ do
			check =<< readTVar vbon
			modifyTVar vb (+ 1)
			writeTVar fl True
		threadDelay 10000
	while $ withNextEvent f \case
		DestroyWindowEvent {} -> False <$ closeField f
		ExposeEvent {} -> True <$ flushField f
		ev@ButtonEvent {} -> True <$ case buttonEvent ev of
			Just BtnEvent {
				buttonNumber = Button1,
				pressOrRelease = Press,
				position = (x, y) } -> atomically do
				writeTVar vbon False
				writeTVar vb 0
				writeTVar clr 0
				writeTVar ul (fromIntegral x, fromIntegral y)
			Just BtnEvent {
				buttonNumber = Button1,
				pressOrRelease = Release } -> atomically $ writeTVar vbon True
			Just BtnEvent {
				buttonNumber = Button2,
				pressOrRelease = Press } -> atomically do
					vo <- readTVar vbon
					when vo $ modifyTVar clr (+ 1) >> writeTVar fl True
			Just BtnEvent {
				buttonNumber = Button3,
				pressOrRelease = Press } -> atomically do
				writeTVar vbon False
				writeTVar vb 0
				(ulx, uly) <- readTVar ul
				(brx, bry) <- readTVar br
				c <- readTVar clr
				let	(x, y, w, h) = calcRect ulx uly brx bry 0
				modifyTVar rcts (((x, y, w, h), c) :)
				writeTVar fl True
			Just _ -> print ev
			Nothing -> error "never occur"
		ev@MotionEvent {} -> True <$ case buttonEvent ev of
			Just BtnEvent {
				buttonNumber = ButtonX,
				pressOrRelease = Move,
				position = (x, y) } -> atomically do
					writeTVar br
						(fromIntegral x, fromIntegral y)
					writeTVar fl True
			_ -> error "never occur"
		ev	| isDeleteEvent f ev -> True <$ destroyField f
			| otherwise -> print ev >> pure True

while :: Monad m => m Bool -> m ()
while act = (`when` while act) =<< act

loop :: Monad m => m a -> m ()
loop act = act >> loop act

calcRect :: Position -> Position -> Position -> Position -> Double ->
	(Position, Position, Dimension, Dimension)
calcRect ulx uly brx bry v = (ulx', uly', fromIntegral w, fromIntegral h)
	where
	ulx' = min ulx brx + round (10 * sin (v / 12))
	uly' = min uly bry
	w = abs $ brx - ulx
	h = abs $ bry - uly
