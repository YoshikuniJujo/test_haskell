{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.CheckRandom where

import Control.Monad.State
import System.Random

import Moffy.React.Common
import Moffy.Handle
import Moffy.EventHandle.Random

instance RandomState StdGen where
	getRandomGen = id
	putRandomGen = flip const

trySimpleRandom :: State StdGen Int
trySimpleRandom = interpretReact (retry handleRandom) getRandom

runStateStdGen :: Int -> State StdGen a -> (a, StdGen)
runStateStdGen n = (`runState` mkStdGen n)

runReactRandom :: React s RandomEv a -> Int -> (a, StdGen)
runReactRandom r n = interpretReact (retry handleRandom) r `runState` mkStdGen n

getRandoms :: Random a => Int -> React s RandomEv [a]
getRandoms n = sequenceA $ n `replicate` getRandom

getRandomRs :: Random a => (a, a) -> Int -> React s RandomEv [a]
getRandomRs mms n = sequenceA $ n `replicate` getRandomR mms

exampleRandoms :: [Int]
exampleRandoms = fst $ getRandomRs (1, 6) 100 `runReactRandom` 8
