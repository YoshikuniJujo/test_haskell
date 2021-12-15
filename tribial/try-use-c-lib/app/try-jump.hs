{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications, RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent.STM

import Human
import Human.MainLoop
import Human.Event

data Jumping = Land | Jumping Int deriving Show
data Run = BackDash | Backward | Stop | Forward | ForwardDash deriving Show

main :: IO ()
main = do
	xv <- atomically $ newTVar 0
	jv <- atomically $ newTVar Land
	rv <- atomically $ newTVar Stop
	f <- fieldNew
	mainLoop \case
		EventEventTick _ -> True <$ do
			atomically $ modifyTVar jv \case
				Land -> Land
				Jumping t
					| t > 99 -> Land
					| otherwise -> Jumping $ t + 1
			atomically $ readTVar rv >>= \case
				BackDash -> modifyTVar xv (subtract 2)
				Backward -> modifyTVar xv (subtract 1)
				Stop -> pure ()
				Forward -> modifyTVar xv (+ 1)
				ForwardDash -> modifyTVar xv (+ 2)
			x <- atomically $ readTVar xv
			fieldClear f
			fieldPutHuman f (x `div` 10) =<< atomically
				((<$> readTVar jv)
					\case Land -> 19; Jumping t -> height t)
			fieldDraw f
		EventEventChar (eventCharToCharacter -> c) ->
			(c /= 113) <$ atomically case c of
				104 -> modifyTVar rv \case
					BackDash -> BackDash
					Backward -> BackDash
					_ -> Backward
				106 -> writeTVar rv Stop
				107 -> readTVar jv >>= \case
					Land -> writeTVar jv $ Jumping 0
					_ -> pure ()
				108 -> readTVar rv >>= \r ->
					writeTVar rv case r of
						ForwardDash -> ForwardDash
						Forward -> ForwardDash
						_ -> Forward
				_ -> pure ()
		_ -> pure False

height :: (Integral m, Integral n) => m -> n
height ((fromIntegral @_ @Double . (`mod` 100)) -> t) =
	19 - round (t * (100 - t) / 400)
