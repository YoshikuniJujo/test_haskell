{-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryHuman where

import Control.Exception

catchAndShow :: forall e . Exception e => IO () -> IO ()
catchAndShow act = act `catch` \(e :: e) -> putStrLn "CATCHED" >> print e
