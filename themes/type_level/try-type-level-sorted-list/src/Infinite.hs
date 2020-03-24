{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Infinite (Infinite(..), head, cycle, take) where

import Prelude hiding (head, take, cycle)

import Data.List.NonEmpty hiding (head, take, cycle)

data Infinite a = a :~ Infinite a

cycle :: NonEmpty a -> Infinite a
cycle ne = ci ne where
	ci (x :| []) = x :~ ci ne
	ci (x :| (y : xs)) = x :~ ci (y :| xs)

head :: Infinite a -> a
head (x :~ _) = x

take :: Int -> Infinite a -> [a]
take n _ | n < 1 = []
take n (x :~ xs) = x : take (n - 1) xs
