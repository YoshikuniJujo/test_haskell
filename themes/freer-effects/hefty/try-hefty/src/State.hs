{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module State where

import Eff qualified as Eff
import Hefty qualified as Hefty
import OpenUnion qualified as Union

data S s a where Get :: S s s; Put :: s -> S s ()

get :: Union.Member (Union.FromFirst (S s)) effs => Eff.E effs s
get = Eff.eff Get

put :: Union.Member (Union.FromFirst (S s)) effs => s -> Eff.E effs ()
put = Eff.eff . Put

run :: Union.HFunctor (Union.U effs) =>
	Eff.E (Union.FromFirst (S s) ': effs) a -> s -> Eff.E effs (a, s)
m `run` s = case m of
	Hefty.Pure x -> Hefty.Pure (x, s)
	u Hefty.:>>= k -> case Union.decomp u of
		Left u' -> Union.hmap (`run` s) (, s) u' Hefty.:>>= \(x, s') -> k x `run` s'
		Right (Union.FromFirst Get) -> k s `run` s
		Right (Union.FromFirst (Put s')) -> k () `run` s'

sample :: (
	Union.Member (Union.FromFirst (S Int)) effs,
	Union.Member (Union.FromFirst IO) effs ) =>
	Eff.E effs ()
sample = do
	put (123 :: Int)
	Eff.eff . print =<< get @Int
