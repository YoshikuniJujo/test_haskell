{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module UseFTCQ.State where

import UseFTCQ.Eff qualified as Eff
import UseFTCQ.HFreer qualified as HFreer
import OpenUnion qualified as Union
import Data.FTCQueue qualified as Q

data S_ s a where Get :: S_ s s; Put :: s -> S_ s ()

type S s = Union.FromFirst (S_ s)

get :: Union.Member (S s) effs => Eff.E effs s
get = Eff.eff Get

put :: Union.Member (S s) effs => s -> Eff.E effs ()
put = Eff.eff . Put

run :: Union.HFunctor (Union.U effs) =>
	Eff.E (S s ': effs) a -> s -> Eff.E effs (a, s)
m `run` s = case m of
	HFreer.Pure x -> HFreer.Pure (x, s)
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> Union.hmap (`run` s) (, s) u' HFreer.:>>=
			Q.singleton \(x, s') -> q `HFreer.app` x `run` s'
		Right (Union.FromFirst Get) -> q `HFreer.app` s `run` s
		Right (Union.ConvertFirst Get k) -> (q `HFreer.app` (k s)) `run` s
		Right (Union.FromFirst (Put s')) -> q `HFreer.app` () `run` s'
		Right (Union.ConvertFirst (Put s') k) -> (q `HFreer.app` (k ())) `run` s'

sample :: (
	Union.Member (S Int) effs,
	Union.Member (Union.FromFirst IO) effs ) =>
	Eff.E effs ()
sample = do
	put (123 :: Int)
	Eff.eff . print =<< get @Int
