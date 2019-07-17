{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Data.Map.Strict

doUntil :: Monad m => m (Maybe a) -> m a
doUntil act = do
	mx <- act
	maybe (doUntil act) return mx

toRoute :: Ord n => [n] -> n -> Map n n -> [n]
toRoute ns n m = case m !? n of
	Just n' -> toRoute (n : ns) n' m
	Nothing -> n : ns
