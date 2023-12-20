{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Samples.Boxes.Run.Gtk3 where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM hiding (retry)

import Control.Moffy
import Control.Moffy.Handle
import Control.Moffy.Samples.Handle.TChan
import Control.Moffy.Samples.View qualified as V
import Control.Moffy.Samples.Run.TChan

import Control.Moffy.Samples.Run.Gtk3

runBoxes b = do
	er <- atomically newTChan
	eo <- atomically newTChan
	v <- atomically newTChan
	void $ forkIO do
		interpret (retry $ handleNew er eo) v do
			b
			emit V.Stopped
	runSingleWin er eo v
