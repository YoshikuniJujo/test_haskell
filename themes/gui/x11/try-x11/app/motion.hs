{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad

import Field
import ButtonEvent

main :: IO ()
main = do
	f <- openField "動き" [exposureMask, buttonPressMask, button1MotionMask]
	while $ withNextEvent f \case
		DestroyWindowEvent {} -> False <$ closeField f
		ExposeEvent {} -> True <$ flushField f
		ev@ButtonEvent {} -> True <$ case buttonEvent ev of
			Just BtnEvent {
				buttonNumber = Button1,
				pressOrRelease = Press,
				position = (x, y) } -> do
					fillRect f 0xff0000
						(fromIntegral x)
						(fromIntegral y) 10 10
					flushField f
			Just _ -> print ev
			Nothing -> error "never occur"
		ev@MotionEvent {} -> True <$ case buttonEvent ev of
			Just BtnEvent {
				buttonNumber = ButtonX,
				pressOrRelease = Move,
				position = (x, y) } -> do
					fillRect f 0x00ff00
						(fromIntegral x)
						(fromIntegral y) 10 10
					flushField f
			_ -> error "never occur"
		ev	| isDeleteEvent f ev -> True <$ destroyField f
			| otherwise -> print ev >> pure True

while :: Monad m => m Bool -> m ()
while act = (`when` while act) =<< act
