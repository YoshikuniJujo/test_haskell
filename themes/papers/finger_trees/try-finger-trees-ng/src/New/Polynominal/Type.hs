{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module New.Polynominal.Type where

import Data.Map.Strict
import Data.Map.Merge.Strict

type Polynominal v = Map (Maybe v) Integer

(.+), (.-) :: Ord v => Polynominal v -> Polynominal v -> Polynominal v
(.+) = merge preserveMissing preserveMissing (zipWithMaybeMatched \_ a b -> removeZero $ a + b)
(.-) = merge preserveMissing (mapMissing \_ b -> negate b) (zipWithMaybeMatched \_ a b -> removeZero $ a - b)

removeZero :: (Eq n, Num n) => n -> Maybe n
removeZero 0 = Nothing
removeZero n = Just n
