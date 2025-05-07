{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeFamilies, GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Lambda where

import Control.Monad
import Data.Functor.Identity
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.HFreer qualified as HFreer
import Yaftee.OpenUnion qualified as Union

import Yaftee.HFunctor qualified as Union
import Data.FTCQueue qualified as Q

data Fun m a b where
	Fun :: (a -> m b) -> Fun m a b

data C m a where
	C :: m a -> C m a

data Calc f i o a where
	Lam :: (C (f i o) t1 -> f i o t2) -> Calc f i o (Fun (f i o) (C (f i o) t1) t2)
	Var :: C (f i o) t -> Calc f i o t
	App :: (Fun (f i o) (C (f i o) t1) t2) -> f i o t1 -> Calc f i o t2

lam :: Union.Member Calc effs =>
	(C (Eff.E effs i o) t1 -> Eff.E effs i o t2) ->
	Eff.E effs i o (Fun (Eff.E effs i o) (C (Eff.E effs i o) t1) t2)
lam = Eff.effh . Lam

var :: Union.Member Calc effs => C (Eff.E effs i o) t -> Eff.E effs i o t
var = Eff.effh . Var

app :: Union.Member Calc effs =>
	(Fun (Eff.E effs i o) (C (Eff.E effs i o) t1) t2) -> Eff.E effs i o t1 -> Eff.E effs i o t2
app = (Eff.effh .) . App

runCbv :: Union.HFunctor (Union.U effs) =>
	Eff.E (Calc ': effs) i o a -> Eff.E effs i o a
runCbv = \case
	HFreer.Pure x -> HFreer.Pure x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> Union.hmap ((Identity <$>) . runCbv) Identity u'
			HFreer.:>>= Q.singleton ((runCbv `HFreer.comp` q) . runIdentity)
		Right (Lam l) -> runCbv $ q `HFreer.app` (Fun l)
		Right (Var (C x)) -> runCbv $ (q `HFreer.app`) =<< x
		Right (App (Fun f) v) -> runCbv do
			a <- v
			v' <- f $ C $ pure a
			q `HFreer.app` v'

runCbn :: Union.HFunctor (Union.U effs) =>
	Eff.E (Calc ': effs) i o a -> Eff.E effs i o a
runCbn = \case
	HFreer.Pure x -> HFreer.Pure x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> Union.hmap ((Identity <$>) . runCbn) Identity u'
			HFreer.:>>= Q.singleton ((runCbn `HFreer.comp` q) . runIdentity)
		Right (Lam l) -> runCbn $ q `HFreer.app` (Fun l)
		Right (Var (C x)) -> runCbn $ (q `HFreer.app`) =<< x
		Right (App (Fun f) v) -> runCbn do
			v' <- f $ C v
			q `HFreer.app` v'

ex :: (
	Union.Member (State.S Int) effs,
	Union.Member Calc effs
	) =>
	Eff.E effs i o Int
ex = do	State.put (1 :: Int)
	f <- lam \x -> do
		n1 <- var x
		n2 <- var x
		pure $ n1 + n2
	v <- f `app` incr
	pure v
	where
	incr = do
		s0 <- State.get
		State.put $ (s0 :: Int) + 1
		s1 <- State.get
		pure s1
