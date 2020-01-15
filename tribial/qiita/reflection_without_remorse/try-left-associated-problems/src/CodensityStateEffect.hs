{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CodensityStateEffect where

import Prelude hiding (abs)

import Eff
import State
import CodensityMonad

sampleL, sampleR :: Member (State Integer) effs => CodensityT (Eff effs) ()
sampleL = foldl (>>) (pure ()) . replicate 8000 . rep $ modify (+ (1 :: Integer))
sampleR = foldr (>>) (pure ()) . replicate 8000 . rep $ modify (+ (1 :: Integer))

sampleLEffect, sampleREffect :: Member (State Integer) effs => Eff effs ()
sampleLEffect = {-# SCC "LeftAssociatedCounter" #-} abs sampleL
sampleREffect = {-# SCC "RightAssociatedCounter" #-} abs sampleR
