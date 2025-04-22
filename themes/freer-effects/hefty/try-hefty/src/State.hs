{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
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

{-
run :: Eff.E (Union.FromFirst (S s) ': effs) a -> s -> Eff.E effs (a, s)
m `run` s = case m of
	Hefty.Pure x -> pure (x, s)
	u Hefty.:>>= k -> case Union.decomp u of
		Left u' -> u' Hefty.:>>= \x -> k x `run` s
		Right (Union.FromFirst Get) -> k s `run` s
		Right (Union.FromFirst (Put s')) -> k () `run` s'
		-}
