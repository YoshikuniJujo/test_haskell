{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.StateNew (

-- * NORMAL

S, get, gets, put, modify, transaction, run,

-- * NAMED

Named, getN, getsN, putN, modifyN, transactionN, runN

) where

import GHC.TypeLits
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.HigherFreer qualified as F
import Control.HigherOpenUnion qualified as Union
import Data.Kind
import Data.HigherFunctor qualified as HFunctor
import Data.Maybe
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

transaction :: forall s ->
	Union.Member (S s) effs => Eff.E effs i o a -> Eff.E effs i o a
transaction = transactionN ""

run :: HFunctor.Loose (Union.U effs) =>
	Eff.E (S s ': effs) i o a -> s -> Eff.E effs i o (a, s)
run = runN

-- * NAMED

{-
type Named nm s = Union.FromFirst (Named_ nm s)

data Named_ (nm :: Symbol) s a where
	Get :: Named_ nm s s; Put :: forall nm s . !s -> Named_ nm s ()
	-}

data Named (nm :: Symbol) s (f :: Type -> Type -> Type -> Type) i o a where
	Get :: forall nm s f i o a . (s -> a) -> Named nm s f i o a
	Put :: forall nm s f i o a . s -> a -> Named nm s f i o a
	Transaction :: forall nm s f i o a . f i o a -> Named nm s f i o a

getN :: forall s effs i o .
	forall nm -> Union.Member (Named nm s) effs => Eff.E effs i o s
getN nm = Eff.effh (Get @nm id)

getsN :: forall s effs i o a . forall nm ->
	Union.Member (Named nm s) effs => (s -> a) -> Eff.E effs i o a
getsN nm f = f <$> getN nm

putN :: forall s effs i o . forall nm ->
	Union.Member (Named nm s) effs => s -> Eff.E effs i o ()
putN nm s = Eff.effh $ Put @nm s ()

modifyN :: forall nm -> Union.Member (Named nm s) effs =>
	(s -> s) -> Eff.E effs i o ()
modifyN nm f = putN nm . f =<< getN nm

transactionN :: forall effs i o a . forall nm s ->
	Union.Member (Named nm s) effs => Eff.E effs i o a -> Eff.E effs i o a
transactionN nm s = Eff.effh . Transaction @nm @s

{-
runN :: forall nm effs s i o a .
	HFunctor.Loose (Union.U effs) =>
	Eff.E (Named nm s ': effs) i o a -> s -> Eff.E effs i o (a, s)
runN = ((uncurry (flip (,)) <$>) .) .  Eff.handleRelayS (,) fst snd \st k s ->
	case st of Get -> k s s; Put s' -> k () s'
	-}

{-
transactionNoGoodN :: forall effs i o a . forall nm s ->
	Union.Member (Named nm s) effs => Eff.E effs i o a -> Eff.E effs i o a
transactionNoGoodN nm s m =
	getN @s nm >>= \s0 ->($ m) . ($ s0) $ fix \go s' -> \case
		F.Pure x -> putN nm s' >> pure x
		u F.:>>= q -> case Union.prj @(Named nm s) u of
			Nothing -> u F.:>>= Q.singleton (go s' F.. q)
			Just (Union.FromFirst Get k) -> go s' $ q F.$ k s'
			Just (Union.FromFirst (Put t) k) -> go t $ q F.$ k ()
			-}

runN :: forall nm effs s i o a .
	HFunctor.Loose (Union.U effs) =>
	Eff.E (Named nm s ': effs) i o a -> s -> Eff.E effs i o (a, s)
m `runN` s = (\(x, y) -> (y, x)) <$> (m `runN_` s)

runN_ :: forall nm effs s i o a .
	HFunctor.Loose (Union.U effs) =>
	Eff.E (Named nm s ': effs) i o a -> s -> Eff.E effs i o (s, a)
m `runN_` s = case m of
	F.Pure x -> F.Pure (s, x)
	u F.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map (`runN_` s) (s ,) u' F.:>>=
			Q.singleton \(s', x) -> (q F.$ x) `runN_` s'
		Right (Get k) -> (q F.$ k s) `runN_` s
		Right (Put s' a) -> (q F.$ a) `runN_` s'
		Right (Transaction a) -> do
			(s', x) <- a `runN'` s
			(q F.$ x) `runN_` fromMaybe s s'

runN' :: forall nm effs s i o a .
	HFunctor.Loose (Union.U effs) =>
	Eff.E (Named nm s ': effs) i o a -> s -> Eff.E effs i o (Maybe s, a)
m `runN'` s = case m of
	F.Pure x -> F.Pure (Just s, x)
	u F.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map (`runN_` s) (s ,) u' F.:>>=
			Q.singleton \(s', x) -> (q F.$ x) `runN'` s'
		Right (Get k) -> (q F.$ k s) `runN'` s
		Right (Put s' a) -> (q F.$ a) `runN'` s'
		Right (Transaction a) -> do
			(s', x) <- a `runN'` s
			(q F.$ x) `runN'` fromMaybe s s'

instance HFunctor.Tight (Named nm s) where
	mapT _ g (Put s x) = Put s $ g x
	mapT _ g (Get k) = Get $ g . k
	mapT f _ (Transaction m) = Transaction $ f m

instance HFunctor.Loose (Named nm s)
