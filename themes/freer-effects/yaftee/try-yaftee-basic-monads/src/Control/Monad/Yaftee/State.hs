{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.State (

	-- * NORMAL

	S, get, put, run,

	-- * NAMED

	Named, getN, putN, runN

	) where

import GHC.TypeLits
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.HigherOpenUnion qualified as Union
import Data.HFunctor qualified as HFunctor

-- NORMAL

type S s = Named "" s

get :: Union.Member (S s) effs => Eff.E effs s
get = getN ""

put :: Union.Member (S s) effs => s -> Eff.E effs ()
put = putN ""

run :: HFunctor.H (Union.U effs) =>
	Eff.E (S s ': effs) a -> s -> Eff.E effs (a, s)
run = runN

-- NAMED

type Named nm s = Union.FromFirst (Named_ nm s)

data Named_ (nm :: Symbol) s a where
	Get :: Named_ nm s s; Put :: forall nm s . !s -> Named_ nm s ()

getN :: forall s effs .
	forall nm -> Union.Member (Named nm s) effs => Eff.E effs s
getN nm = Eff.eff (Get @nm)

putN :: forall s effs .
	forall nm -> Union.Member (Named nm s) effs => s -> Eff.E effs ()
putN nm = Eff.eff . (Put @nm)

runN :: HFunctor.H (Union.U effs) =>
	Eff.E (Named nm s ': effs) a -> s -> Eff.E effs (a, s)
runN = Eff.handleRelayS (,) fst snd \st k s ->
	case st of Get -> k s s; Put s' -> k () s'

	{-
m `runN` s = case m of
	HFreer.Pure x -> HFreer.Pure (x, s)
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map (`runN` s) (, s) u' HFreer.:>>=
			Q.singleton \(x, s') -> q `HFreer.app` x `runN` s'
		Right (Union.FromFirst Get k) -> q `HFreer.app` (k s) `runN` s
		Right (Union.FromFirst (Put s') k) -> q `HFreer.app` (k ()) `runN` s'
		-}
