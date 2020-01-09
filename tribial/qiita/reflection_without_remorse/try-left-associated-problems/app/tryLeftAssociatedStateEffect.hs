{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Eff
import State

main :: IO ()
main = do
	print . run $ sampleL `runState` (0 :: Integer)
	print . run $ sampleR `runState` (0 :: Integer)

sampleL, sampleR :: Member (State Integer) effs => Eff effs ()
sampleL = {-# SCC "LeftAssociatedCounter" #-}
	foldl (>>) (pure ()) . replicate 8000 $ modify (+ (1 :: Integer))
sampleR = {-# SCC "RightAssociatedCounter" #-}
	foldr (>>) (pure ()) . replicate 8000 $ modify (+ (1 :: Integer))
