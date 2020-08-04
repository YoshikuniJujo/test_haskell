{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.CheckRandom where

import Control.Monad
import Control.Moffy
import Control.Moffy.Handle
import Control.Moffy.Run
import Data.Functor.Identity
import System.Random

import Control.Moffy.Event.Random
import Control.Moffy.Handle.Random

---------------------------------------------------------------------------
-- COMMON
---------------------------------------------------------------------------

getRandoms :: Random a => Int -> React s RandomEv [a]
getRandoms n = n `replicateM` getRandom

getRandomRs :: Random a => (a, a) -> Int -> React s RandomEv [a]
getRandomRs mms n = n `replicateM` getRandomR mms

---------------------------------------------------------------------------
-- NEW HANDLE
---------------------------------------------------------------------------

trySimpleRandom' :: StdGen -> Identity (Int, StdGen)
trySimpleRandom' = interpretReactSt (retrySt handleRandom) getRandom

runStateStdGen' :: Int -> St StdGen m a -> m (a, StdGen)
runStateStdGen' n = ($ mkStdGen n)

runReactRandom' :: Monad m => React s RandomEv a -> Int -> m (a, StdGen)
runReactRandom' r n = interpretReactSt (retrySt handleRandom) r $ mkStdGen n

exampleRandoms' :: [Int]
exampleRandoms' = runIdentity $ fst <$> getRandomRs (1, 6) 100 `runReactRandom'` 8
