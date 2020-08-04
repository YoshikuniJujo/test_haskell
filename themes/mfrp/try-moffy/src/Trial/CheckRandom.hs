{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.CheckRandom where

import Control.Monad
import Control.Monad.State
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
-- OLD HANDLE
---------------------------------------------------------------------------

trySimpleRandom :: State StdGen Int
trySimpleRandom = interpretReact (retry handleRandom) getRandom

runStateStdGen :: Int -> State StdGen a -> (a, StdGen)
runStateStdGen n = (`runState` mkStdGen n)

runReactRandom :: React s RandomEv a -> Int -> (a, StdGen)
runReactRandom r n = interpretReact (retry handleRandom) r `runState` mkStdGen n

exampleRandoms :: [Int]
exampleRandoms = fst $ getRandomRs (1, 6) 100 `runReactRandom` 8

---------------------------------------------------------------------------
-- NEW HANDLE
---------------------------------------------------------------------------

trySimpleRandom' :: StdGen -> Identity (Int, StdGen)
trySimpleRandom' = interpretReactSt (retrySt handleRandom') getRandom

runStateStdGen' :: Int -> St StdGen m a -> m (a, StdGen)
runStateStdGen' n = ($ mkStdGen n)

runReactRandom' :: Monad m => React s RandomEv a -> Int -> m (a, StdGen)
runReactRandom' r n = interpretReactSt (retrySt handleRandom') r $ mkStdGen n

exampleRandoms' :: [Int]
exampleRandoms' = runIdentity $ fst <$> getRandomRs (1, 6) 100 `runReactRandom'` 8
