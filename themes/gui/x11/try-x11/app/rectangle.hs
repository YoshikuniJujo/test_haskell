{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

import Field
import ButtonEvent

main :: IO ()
main = do
	(ul, br, fl) <- atomically
		$ (,,) <$> newTVar (0, 0) <*> newTVar (0, 0) <*> newTVar False
	f <- openField "長方形" [
		exposureMask, buttonPressMask, button1MotionMask ]
	void . forkIO $ loop do
		((ulx_, uly_), (brx_, bry_)) <- atomically $ do
			check =<< readTVar fl <* writeTVar fl False
			(,) <$> readTVar ul <*> readTVar br
		let	(ulx, uly, w, h) = calcRect ulx_ uly_ brx_ bry_
		clearField f >> fillRect f 0x0000ff ulx uly w h >> flushField f
	while $ withNextEvent f \case
		DestroyWindowEvent {} -> False <$ closeField f
		ExposeEvent {} -> True <$ flushField f
		ev@ButtonEvent {} -> True <$ case buttonEvent ev of
			Just BtnEvent {
				buttonNumber = Button1,
				pressOrRelease = Press,
				position = (x, y) } -> atomically $ writeTVar ul
					(fromIntegral x, fromIntegral y)
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

calcRect :: Position -> Position -> Position -> Position ->
	(Position, Position, Dimension, Dimension)
calcRect ulx uly brx bry = (ulx', uly', fromIntegral w, fromIntegral h)
	where
	ulx' = min ulx brx
	uly' = min uly bry
	w = abs $ brx - ulx
	h = abs $ bry - uly
