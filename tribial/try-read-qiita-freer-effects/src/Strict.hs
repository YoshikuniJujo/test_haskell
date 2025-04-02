{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Strict where

import Control.Concurrent
import System.IO.Unsafe

import Coyoneda

data StrictMaybe a = StrictNothing | StrictJust !a deriving Show

instance Functor StrictMaybe where
	fmap f = \case
		StrictNothing -> StrictNothing
		StrictJust x -> StrictJust $ f x

isStrictJust :: StrictMaybe a -> Bool
isStrictJust = \case StrictNothing -> False; StrictJust _ -> True

slowSucc :: Integer -> Integer
slowSucc n = unsafePerformIO $ threadDelay 1000000 >> return (succ n)

headIfAllJust :: [StrictMaybe a] -> StrictMaybe a
headIfAllJust ma@(m : _) | all isStrictJust ma = m
headIfAllJust _ = StrictNothing

headIfAllJustCoyoneda :: [Coyoneda StrictMaybe a] -> Coyoneda StrictMaybe a
headIfAllJustCoyoneda ma@(m : _) | all (fromContext isStrictJust) ma = m
headIfAllJustCoyoneda _ = coyoneda StrictNothing
