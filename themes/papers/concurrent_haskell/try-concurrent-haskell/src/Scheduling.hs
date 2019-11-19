{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Scheduling where

import Control.Concurrent

type QSem' = MVar (Int, [MVar ()])

newQSem :: IO QSem'
newQSem = newMVar (0, [])

waitQSem :: QSem' -> IO ()
waitQSem sem =
	takeMVar sem >>= \(avail, blkd) ->
	if avail > 0 then putMVar sem (avail - 1, []) else
		newMVar () >>= \blk ->
		putMVar sem (0, blk : blkd) >>
		takeMVar blk

signalQSem :: QSem' -> IO ()
signalQSem sem =
	takeMVar sem >>= \(avail, blkd) ->
	case blkd of
		[] -> putMVar sem (avail + 1, [])
		blk : blkd' -> putMVar sem (0, blkd') >> putMVar blk ()
