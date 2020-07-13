{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.CheckRandom where

import Control.Monad
import Control.Monad.State
import Control.Moffy
import Control.Moffy.Handle
import Control.Moffy.Run
import System.Random

import Control.Moffy.Event.Random
import Control.Moffy.Handle.Random

trySimpleRandom :: State StdGen Int
trySimpleRandom = interpretReact (retry handleRandom) getRandom

runStateStdGen :: Int -> State StdGen a -> (a, StdGen)
runStateStdGen n = (`runState` mkStdGen n)

runReactRandom :: React s RandomEv a -> Int -> (a, StdGen)
runReactRandom r n = interpretReact (retry handleRandom) r `runState` mkStdGen n

getRandoms :: Random a => Int -> React s RandomEv [a]
getRandoms n = n `replicateM` getRandom

getRandomRs :: Random a => (a, a) -> Int -> React s RandomEv [a]
getRandomRs mms n = n `replicateM` getRandomR mms

exampleRandoms :: [Int]
exampleRandoms = fst $ getRandomRs (1, 6) 100 `runReactRandom` 8
