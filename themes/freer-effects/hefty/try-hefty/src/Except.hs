{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGe FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Except where

import Control.Monad
import Eff qualified as Eff
import Hefty qualified as Hefty
import OpenUnion qualified as Union

data E e f a where
	Throw :: e -> E e f a
	Catch :: f a -> (e -> f a) -> E e f a

throw :: Union.Member (E e) effs => e -> Eff.E effs a
throw = Eff.effh . Throw

catch :: Union.Member (E e) effs =>
	Eff.E effs a -> (e -> Eff.E effs a) -> Eff.E effs a
m `catch` h = Eff.effh $ Catch m h

run :: Union.HFunctor (Union.U effs) => Eff.E (E e ': effs) a -> Eff.E effs (Either e a)
run = \case
	Hefty.Pure x -> Hefty.Pure $ Right x
	u Hefty.:>>= k -> case Union.decomp u of
		Left u' -> Union.hmap run Right u' Hefty.:>>= either (Hefty.Pure . Left) (run . k)
		Right (Throw e) -> Hefty.Pure $ Left e
		Right (m `Catch` h) -> either (Hefty.Pure . Left) (run . k)
			=<< either (run . h) (Hefty.Pure . Right) =<< run m

sample :: (
	Union.Member (E String) effs,
	Union.Member (Union.FromFirst IO) effs
	) =>
	Int -> Int -> Eff.E effs ()
sample m n = do
	when (n == 0) $ throw "zero division"
	Eff.eff . print $ m `div` n

sample' m n = sample m n `catch` \e -> Eff.eff . putStrLn $ "catched! message is `" ++ e ++ "'"
