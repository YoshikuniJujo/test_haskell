{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Human.MainLoop where

import Control.Monad.Fix
import Control.Exception
import Data.Bool
import System.IO

import Human.Event

mainLoop :: (forall s . Event s -> IO Bool) -> IO ()
mainLoop f = hGetBuffering stdin >>= \bm -> finally (hSetBuffering stdin bm) do
	hSetBuffering stdin NoBuffering
	go . (`withEvent` f) =<< hGetAndPushCChar stdin
	where go = fix \l act -> bool (pure ()) (l act) =<< act
