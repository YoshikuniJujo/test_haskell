{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.State (

-- * NORMAL

S, get, gets, put, modify, run,

-- * NAMED

Named, getN, getsN, putN, modifyN, runN

) where

import GHC.TypeLits
import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.HigherFreer qualified as HFreer
import Control.HigherOpenUnion qualified as Union
import Data.HigherFunctor qualified as HFunctor
import Data.FTCQueue qualified as Q

-- * NORMAL

type S s = Named "" s

get :: Union.Member (S s) effs => Eff.E effs i o s
get = getN ""

gets :: Union.Member (S s) effs => (s -> a) -> Eff.E effs i o a
gets = getsN ""

put :: Union.Member (S s) effs => s -> Eff.E effs i o ()
put = putN ""

modify :: Union.Member (S s) effs => (s -> s) -> Eff.E effs i o ()
modify = modifyN ""

run :: HFunctor.Loose (Union.U effs) =>
	Eff.E (S s ': effs) i o a -> s -> Eff.E effs i o (a, s)
run = runN

-- * NAMED

type Named nm s = Union.FromFirst (Named_ nm s)

data Named_ (nm :: Symbol) s a where
	Get :: Named_ nm s s; Put :: forall nm s . !s -> Named_ nm s ()

getN :: forall s effs i o .
	forall nm -> Union.Member (Named nm s) effs => Eff.E effs i o s
getN nm = Eff.eff (Get @nm)

getsN :: forall s effs i o a . forall nm ->
	Union.Member (Named nm s) effs => (s -> a) -> Eff.E effs i o a
getsN nm f = f <$> getN nm

putN :: forall s effs i o . forall nm ->
	Union.Member (Named nm s) effs => s -> Eff.E effs i o ()
putN nm = Eff.eff . Put @nm

modifyN :: forall nm -> Union.Member (Named nm s) effs =>
	(s -> s) -> Eff.E effs i o ()
modifyN nm f = putN nm . f =<< getN nm

runN :: HFunctor.Loose (Union.U effs) =>
	Eff.E (Named nm s ': effs) i o a -> s -> Eff.E effs i o (a, s)
runN = ((uncurry (flip (,)) <$>) .) .  Eff.handleRelayS (,) fst snd \st k s ->
	case st of Get -> k s s; Put s' -> k () s'

transactionNoGoodN :: forall effs i o a . forall nm s ->
	Union.Member (Named nm s) effs => Eff.E effs i o a -> Eff.E effs i o a
transactionNoGoodN nm s m =
	getN @s nm >>= \s0 ->($ m) . ($ s0) $ fix \go s' -> \case
		HFreer.Pure x -> putN nm s' >> pure x
		u HFreer.:>>= q -> case Union.prj @(Named nm s) u of
			Nothing -> u HFreer.:>>=
				Q.singleton (go s' `HFreer.comp` q)
			Just (Union.FromFirst Get k) ->
				go s' $ q `HFreer.app` k s'
			Just (Union.FromFirst (Put t) k) ->
				go t $ q `HFreer.app` k ()
