{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.TimeTChan where

import Control.Monad.State
import Control.Moffy
import Control.Moffy.Event.Time
import Control.Concurrent
import Control.Concurrent.STM
import Data.Type.Set
import Data.OneOrMore
import Data.Time
import Data.Time.Clock.TAI
import Data.Time.Clock.System

handleDeltaTimeTChan :: DiffTime -> IO (TChan (EvOccs (Singleton DeltaTime)))
handleDeltaTimeTChan t = do
	c <- newTChanIO
	c <$ ((handleDeltaTimeTChanSt t c `evalStateT`) =<< systemToTAITime <$> getSystemTime)

handleDeltaTimeTChanSt :: DiffTime -> TChan (EvOccs (Singleton DeltaTime)) -> StateT AbsoluteTime IO ()
handleDeltaTimeTChanSt t c = forever do
	lift . threadDelay . round $ t * 1000000
	lift . atomically . writeTChan c . Singleton . OccDeltaTime =<< diffTime

diffTime :: StateT AbsoluteTime IO DiffTime
diffTime = do
	t <- get
	now <- systemToTAITime <$> lift getSystemTime
	now `diffAbsoluteTime` t <$ put now
