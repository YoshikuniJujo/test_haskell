{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryHuman where

import Control.Monad.ST
import Control.Exception

import Human

catchAndShow :: forall e . Exception e => IO () -> IO ()
catchAndShow act = act `catch` \(e :: e) -> putStrLn "CATCHED" >> print e

image1 :: Image
image1 = runST do
	f <- fieldNewSt
	fieldPutHumanSt f 30 10
	fieldGetImageSt f
