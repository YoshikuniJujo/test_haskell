{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Tips (loopIf, doWhile) where

loopIf :: Monad m => m Bool -> m ()
loopIf act = do
	b <- act
	if b then loopIf act else return ()

doWhile :: Monad m => m (Maybe a) -> m a
doWhile act = maybe (doWhile act) pure =<< act
