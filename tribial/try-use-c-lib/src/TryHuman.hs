{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryHuman where

import Control.Exception
import Human.Exception

catchPartial :: IO () -> IO ()
catchPartial act = act `catch` \(e :: PutHumanPartialError) ->
	putStrLn "CATCHED" >> print e

catchOffscreen :: IO () -> IO ()
catchOffscreen act = act `catch` \(e :: PutHumanOffscreenError) ->
	putStrLn "CATCHED" >> print e

catchOutOfField :: IO () -> IO ()
catchOutOfField act = act `catch` \(e :: PutHumanOutOfFieldError) -> do
	putStrLn "CATCHED" >> print e
