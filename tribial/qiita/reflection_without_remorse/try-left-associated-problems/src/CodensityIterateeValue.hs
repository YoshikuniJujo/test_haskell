{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module CodensityIterateeValue where

import Prelude hiding (abs)
import Control.Monad

import IterateeValue
import CodensityMonad

type ItvCo i o a = CodensityT (Itv i o) a

getvCo :: o -> ItvCo i o i
getvCo = rep . getv

feedAllCo :: ItvCo i o a -> [i] -> ([o], Maybe a)
feedAllCo = feedAll . abs

feedPartialCo :: Int -> ItvCo i o a -> ItvCo i o (ItvCo i o a)
feedPartialCo n m = rep <$> rep (feedPartial n $ abs m)

addNCo :: Int -> ItvCo Int Int Int
addNCo n = foldl (>=>) pure (replicate n $ rep . addGet) 0

tryR, tryCo :: [Int]
tryR = fst $ feedPartial 2000000 (addNR 4000000) `feedAll` [1 .. 10]
tryCo = fst $ feedPartialCo 2000000 (addNCo 4000000) `feedAllCo` [1 .. 10]
