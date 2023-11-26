{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.TChan (handle, handleNew) where

import Control.Moffy
import Control.Moffy.Handle
import Control.Concurrent.STM
import Data.Time
import System.Timeout

handle :: Maybe DiffTime -> TChan (EvReqs es) -> TChan (EvOccs es) -> Handle' IO es
handle mt cr c rqs = maybe (Just <$>) (timeout . round . (* 1000000)) mt do
	atomically $ writeTChan cr rqs
	atomically $ readTChan c

handleNew :: TChan (EvReqs es) -> TChan (EvOccs es) -> Handle' IO es
handleNew cer ceo rqs = do
	atomically $ writeTChan cer rqs
	atomically $ tryReadTChan ceo
