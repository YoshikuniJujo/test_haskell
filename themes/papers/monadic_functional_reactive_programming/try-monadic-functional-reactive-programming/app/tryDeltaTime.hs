{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.State
import Data.Set
import Data.Time

import React
import MouseAndTime
import GuiEv

import Field

main :: IO ()
main = do
	f <- openField "時間の経過" [exposureMask, buttonPressMask]
	t <- getCurrentTime
	(interpret (handle f) deltaTime `runStateT` t) >>= print
	closeField f

handle :: Field -> EvReqs GuiEv -> StateT UTCTime IO (EvOccs GuiEv)
handle f r = do
	t <- get
	n <- liftIO getCurrentTime
	put n
	withNextEventTimeout f 50000 \case
		[] -> pure . singleton . DeltaTime . Occurred $ n `diffUTCTime` t
		_ -> handle f r
