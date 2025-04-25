{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
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

	S, get, gets, put, modify, run, transaction,

	-- * NAMED

	Named, getN, getsN, putN, modifyN, runN, transactionN

	) where

import GHC.TypeLits
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.HFreer qualified as HFreer
import Control.HigherOpenUnion qualified as Union
import Data.HFunctor qualified as HFunctor
import Data.FTCQueue qualified as Q

-- NORMAL

type S s = Named "" s

get :: Union.Member (S s) effs => Eff.E effs s
get = getN ""

gets :: Union.Member (S s) effs => (s -> a) -> Eff.E effs a
gets = getsN ""

put :: Union.Member (S s) effs => s -> Eff.E effs ()
put = putN ""

modify :: Union.Member (S s) effs => (s -> s) -> Eff.E effs ()
modify = modifyN ""

run :: HFunctor.H (Union.U effs) =>
	Eff.E (S s ': effs) a -> s -> Eff.E effs (a, s)
run = runN

transaction :: forall effs a . forall s -> Union.Member (S s) effs =>
	Eff.E effs a -> Eff.E effs a
transaction = transactionN ""

-- NAMED

type Named nm s = Union.FromFirst (Named_ nm s)

data Named_ (nm :: Symbol) s a where
	Get :: Named_ nm s s; Put :: forall nm s . !s -> Named_ nm s ()

getN :: forall s effs .
	forall nm -> Union.Member (Named nm s) effs => Eff.E effs s
getN nm = Eff.eff (Get @nm)

getsN :: forall s effs a . forall nm ->
	(Union.Member (Named nm s) effs) => (s -> a) -> Eff.E effs a
getsN nm f = f <$> getN nm

putN :: forall s effs .
	forall nm -> Union.Member (Named nm s) effs => s -> Eff.E effs ()
putN nm = Eff.eff . (Put @nm)

modifyN :: forall s effs . forall nm -> Union.Member (Named nm s) effs =>
	(s -> s) -> Eff.E effs ()
modifyN nm f = putN nm . f =<< getN nm

runN :: HFunctor.H (Union.U effs) =>
	Eff.E (Named nm s ': effs) a -> s -> Eff.E effs (a, s)
runN = Eff.handleRelayS (,) fst snd \st k s ->
	case st of Get -> k s s; Put s' -> k () s'

transactionN :: forall effs a . forall nm s ->
	Union.Member (Named nm s) effs => Eff.E effs a -> Eff.E effs a
transactionN nm s m = getN @s nm >>= \s0 -> ($ m) . ($ s0) $ fix \go s' -> \case
	HFreer.Pure x -> putN nm s' >> pure x
	u HFreer.:>>= q -> case Union.prj @(Named nm s) u of
		Nothing -> u HFreer.:>>= Q.singleton (go s' `HFreer.comp` q)
		Just (Union.FromFirst Get k) -> go s' $ q `HFreer.app` k s'
		Just (Union.FromFirst (Put t) k) -> go t $ q `HFreer.app` k ()
