{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Tips (doWhile) where

doWhile :: Monad m => m (Maybe a) -> m a
doWhile act = maybe (doWhile act) pure =<< act
