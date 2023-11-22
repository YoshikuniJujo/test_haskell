{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryRandom (
	-- * Example Random Number List
	diceTrialList ) where

import Control.Monad (replicateM)
import Control.Moffy (React)
import Control.Moffy.Event.Random (RandomEv, getRandomR)
import Control.Moffy.Handle (retrySt)
import Control.Moffy.Handle.Random (handleRandom)
import Control.Moffy.Run (interpretReactSt)
import Data.Functor.Identity (runIdentity)
import System.Random (Random, StdGen, mkStdGen)

---------------------------------------------------------------------------

diceTrialList :: [Int]
diceTrialList = runIdentity $ getRandomRs (1, 6) 100 `evalRandom` mkStdGen 8

evalRandom :: Monad m => React s RandomEv a -> StdGen -> m a
evalRandom  r = (fst <$>) . interpretReactSt (retrySt handleRandom) r

getRandomRs :: Random a => (a, a) -> Int -> React s RandomEv [a]
getRandomRs = flip replicateM . getRandomR
