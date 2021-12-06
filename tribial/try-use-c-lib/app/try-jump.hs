{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent.STM
import Foreign.C.Types

import Human
import Human.MainLoop
import Human.Event

main :: IO ()
main = do
	xv <- atomically $ newTVar 0
	f <- fieldNew
	mainLoop \case
		EventEventTick evt -> True <$ do
			fieldClear f
--			fieldPutHuman f (eventTickToTimes evt `div` 10 `mod` 70) 15
--			fieldPutHuman f ((- eventTickToTimes evt) `div` 10 `mod` 70) 15
			x <- atomically $ readTVar xv
			fieldPutHuman f x . calcHeight $ eventTickToTimes evt
			fieldPutHuman f (eventTickToTimes evt `div` 10 `mod` 70)
				. calcHeight $ eventTickToTimes evt
			fieldDraw f
		EventEventChar evc -> do
--			print evc
			case eventCharToCharacter evc of
				104 -> atomically $ modifyTVar xv $ subtract 1
				108 -> atomically $ modifyTVar xv (+ 1)
				_ -> pure ()
			pure (eventCharToCharacter evc /= 113)
		ev -> False <$ print ev

calcHeight :: CInt -> CInt
calcHeight ((fromIntegral . (`mod` 100)) -> t) =
	19 - round (t * (100 - t) / 400)
