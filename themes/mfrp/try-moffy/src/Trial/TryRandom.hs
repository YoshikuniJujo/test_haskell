{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.TryRandom (
	-- * Example Random Number List
	exampleRandomRs ) where

import Control.Monad (replicateM)
import Control.Moffy (React)
import Control.Moffy.Event.Random (RandomEv, getRandomR)
import Control.Moffy.Handle (retrySt)
import Control.Moffy.Handle.Random (handleRandom)
import Control.Moffy.Run (interpretReactSt)
import Data.Functor.Identity (runIdentity)
import System.Random (Random, mkStdGen)

---------------------------------------------------------------------------

exampleRandomRs :: [Int]
exampleRandomRs = runIdentity $ getRandomRs (1, 6) 100 `evalRandom` 8

evalRandom :: Monad m => React s RandomEv a -> Int -> m a
evalRandom r n = fst <$> interpretReactSt (retrySt handleRandom) r (mkStdGen n)

getRandomRs :: Random a => (a, a) -> Int -> React s RandomEv [a]
getRandomRs = flip replicateM . getRandomR
