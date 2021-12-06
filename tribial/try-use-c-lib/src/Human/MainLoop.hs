{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Human.MainLoop where

import Control.Exception
import Data.Bool
import System.IO

import Human.Event

mainLoop :: (forall s . Event s -> IO Bool) -> IO ()
mainLoop f = do
	bfm <- hGetBuffering stdin
	finally (hSetBuffering stdin bfm) do
		hSetBuffering stdin NoBuffering
		ch <- hGetAndPushCChar stdin
		doWhile_ $ withEvent ch f

doWhile_ :: Monad m => m Bool -> m ()
doWhile_ act = bool (pure ()) (doWhile_ act) =<< act
