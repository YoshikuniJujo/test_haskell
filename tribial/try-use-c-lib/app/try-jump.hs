{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications, RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent.STM

import Human
import Human.MainLoop
import Human.Event

data Jumping = NotJump | Jumping Int deriving Show
data Run = BackDash | Backward | Stop | Forward | ForwardDash deriving Show

main :: IO ()
main = do
	xv <- atomically $ newTVar 0
	jv <- atomically $ newTVar NotJump
	rv <- atomically $ newTVar Stop
	f <- fieldNew
	mainLoop \case
		EventEventTick evt -> True <$ do
			fieldClear f
--			fieldPutHuman f (eventTickToTimes evt `div` 10 `mod` 70) 15
--			fieldPutHuman f ((- eventTickToTimes evt) `div` 10 `mod` 70) 15
			x <- atomically $ readTVar xv
			j <- atomically $ readTVar jv
			let	h = case j of
					NotJump -> 19
					Jumping t -> calcHeight t
			atomically $ modifyTVar jv \case
				NotJump -> NotJump
				Jumping t
					| t > 99 -> NotJump
					| otherwise -> Jumping $ t + 1
			atomically do
				r <- readTVar rv
				case r of
					BackDash -> modifyTVar xv (subtract 2)
					Backward -> modifyTVar xv (subtract 1)
					Stop -> pure ()
					Forward -> modifyTVar xv (+ 1)
					ForwardDash -> modifyTVar xv (+ 2)
			fieldPutHuman f (x `div` 10) $ fromIntegral h -- . calcHeight $ eventTickToTimes evt
			fieldPutHuman f (eventTickToTimes evt `div` 10 `mod` 70) -- $ fromIntegral h
				. calcHeight $ eventTickToTimes evt
			fieldDraw f
		EventEventChar evc -> do
--			print evc
			case eventCharToCharacter evc of
				104 -> atomically do
					r <- readTVar rv
					writeTVar rv case r of
						BackDash -> BackDash
						Backward -> BackDash
						_ -> Backward
				108 -> atomically do
					r <- readTVar rv
					writeTVar rv case r of
						ForwardDash -> ForwardDash
						Forward -> ForwardDash
						_ -> Forward
				106 -> atomically $ writeTVar rv Stop
				107 -> atomically do
					j <- readTVar jv
					case j of
						NotJump -> writeTVar jv $ Jumping 0
						_ -> pure ()
				_ -> pure ()
			pure (eventCharToCharacter evc /= 113)
		ev -> False <$ print ev

calcHeight :: Integral n => n -> n
calcHeight ((fromIntegral @_ @Double . (`mod` 100)) -> t) =
	19 - round (t * (100 - t) / 400)
