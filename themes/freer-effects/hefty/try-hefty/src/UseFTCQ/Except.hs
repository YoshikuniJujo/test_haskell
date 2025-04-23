{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseFTCQ.Except where

import Control.Monad
import UseFTCQ.Eff qualified as Eff
import UseFTCQ.HFreer qualified as HFreer
import OpenUnion qualified as Union
import Data.FTCQueue qualified as Q

data E e f a where
	Throw :: e -> E e f a
	Catch :: f a -> (e -> f a) -> E e f a

throw :: Union.Member (E e) effs => e -> Eff.E effs a
throw = Eff.effh . Throw

catch :: Union.Member (E e) effs =>
	Eff.E effs a -> (e -> Eff.E effs a) -> Eff.E effs a
catch = (Eff.effh .) . Catch

run :: Union.HFunctor (Union.U effs) =>
	Eff.E (E e ': effs) a -> Eff.E effs (Either e a)
run = \case
	HFreer.Pure x -> HFreer.Pure $ Right x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> Union.hmap run Right u' HFreer.:>>=
			Q.singleton (either (HFreer.Pure . Left) (run `HFreer.comp` q))
		Right (Throw e) -> HFreer.Pure $ Left e
		Right (m `Catch` h) -> either (HFreer.Pure . Left) (run `HFreer.comp` q)
			=<< either (run . h) (HFreer.Pure . Right) =<< run m

sample :: (
	Union.Member (E String) effs,
	Union.Member (Union.FromFirst IO) effs ) =>
	Int -> Int -> Eff.E effs ()
sample m n = do
	when (n == 0) $ throw "zero division"
	Eff.eff . print $ m `div` n

sample' m n = sample m n `catch` \e -> Eff.eff . putStrLn $ "catched! message is `" ++ e ++ "'"
