{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Except where

import Control.Monad
import Data.Kind

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.HFreer qualified as HFreer
import Yaftee.OpenUnion qualified as Union
import Yaftee.HFunctor qualified as Union
import Data.FTCQueue qualified as Q

data E e f i o a where
	Throw :: e -> E e f i o a
	Catch :: f i o a -> (e -> f i o a) -> E e f i o a

throw :: Union.Member (E e) effs => e -> Eff.E effs i o a
throw = Eff.effh . Throw

m `catch` h = Eff.effh $ m `Catch` h

run :: Union.HFunctor (Union.U effs) =>
	Eff.E (E e ': effs) i o a -> Eff.E effs i o (Either e a)
run = \case
	HFreer.Pure x -> HFreer.Pure $ Right x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> Union.hmap run Right u' HFreer.:>>= Q.singleton
			(either (HFreer.Pure . Left) (run `HFreer.comp` q))
		Right (Throw e) -> HFreer.Pure $ Left e
		Right (m `Catch` h) ->
			either (HFreer.Pure . Left) (run `HFreer.comp` q)
				=<< either (run . h) (HFreer.Pure . Right)
				=<< run m

instance Union.HFunctor (E e) where
	hmap _ _ (Throw e) = Throw e
	hmap f _ (m `Catch` h) = f m `Catch` (f . h)

sample :: (
	Union.Member (E String) effs,
	Union.Member (Union.FromFirst IO) effs ) =>
	Int -> Int -> Eff.E effs i o ()
sample m n = do
	when (n == 0) $ throw "zero devision"
	Eff.eff . print $ m `div` n

sample' m n = sample m n `catch` \e -> Eff.eff . putStrLn $ "cathed! mesage is `" ++ e ++ "'"
