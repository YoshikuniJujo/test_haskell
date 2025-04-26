{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.State where

import Yaftee.Eff qualified as Eff
import Yaftee.HFreer qualified as HFreer
import Yaftee.OpenUnion qualified as Union

type S s = Union.FromFirst (S_ s)
data S_ s a where Get :: S_ s s; Put :: s -> S_ s ()

get :: Union.Member (S s) effs => Eff.E effs i o s
get = Eff.eff Get

put :: Union.Member (S s) effs => s -> Eff.E effs i o ()
put = Eff.eff . Put

modify f = put . f =<< get

run :: Union.HFunctor (Union.U effs) =>
	Eff.E (S s ': effs) i o a -> s -> Eff.E effs i o (a, s)
m `run` s = case m of
	HFreer.Pure x -> HFreer.Pure (x, s)
	u HFreer.:>>= k -> case Union.decomp u of
		Left u' -> Union.hmap (`run` s) (, s) u' HFreer.:>>= \(x, s') -> k x `run` s'
		Right (Union.FromFirst Get k') -> k (k' s) `run` s
		Right (Union.FromFirst (Put s') k') -> k (k' ()) `run` s'

sample :: (
	Union.Member (S Int) effs,
	Union.Member (Union.FromFirst IO) effs ) => Eff.E effs i o ()
sample = do
	put (123 :: Int)
	Eff.eff . print =<< get @Int
