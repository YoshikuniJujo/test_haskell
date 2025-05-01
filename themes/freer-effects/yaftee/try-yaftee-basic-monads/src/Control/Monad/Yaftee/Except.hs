{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ExplicitForAll, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Except (

-- * NORMAL

E, throw, catch, run, runExc, runIO,

-- * NAMED

Named, throwN, catchN, runN, runExcN, runION

) where

import GHC.TypeLits
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.IO qualified as IO
import Control.Monad.HigherFreer qualified as F
import Control.HigherOpenUnion qualified as Union
import Control.Exception qualified as IO
import Data.Kind
import Data.Functor.Identity
import Data.HigherFunctor qualified as HFunctor
import Data.FTCQueue qualified as Q

-- * NORMAL

type E = Named ""

throw :: Union.Member (E e) effs => e -> Eff.E effs i o a
throw = throwN ""

catch :: Union.Member (E e) effs =>
	Eff.E effs i o a -> (e -> Eff.E effs i o a) -> Eff.E effs i o a
catch = catchN ""

run :: forall e effs i o a . HFunctor.Loose (Union.U effs) =>
	Eff.E (E e ': effs) i o a -> Eff.E effs i o (Either e a)
run = runN

runExc :: forall e e' effs i o a .
	(HFunctor.Loose (Union.U effs), Union.Member (E e') effs) =>
	(e -> e') -> (e' -> e) -> Eff.E (E e ': effs) i o a -> Eff.E effs i o a
runExc = runExcN ""

runIO :: IO.Exception e => Eff.E '[E e, IO.I] i o a -> Eff.E '[IO.I] i o a
runIO = runION

-- * NAMED

data Named (nm :: Symbol) e (f :: Type -> Type -> Type -> Type) i o a where
	Throw :: forall nm e f a i o . e -> Named nm e f i o a
	Catch :: forall nm e f a i o .
		f i o a -> (e -> f i o a) -> Named nm e f i o a

instance HFunctor.Tight (Named nm e) where
	mapT _ _ (Throw e) = Throw e
	mapT f _ (m `Catch` h) = (f m) `Catch` \e -> f $ h e

instance HFunctor.Loose (Named nm e)

throwN :: forall nm -> Union.Member (Named nm e) effs => e -> Eff.E effs i o a
throwN nm = Eff.effh . Throw @nm

catchN :: forall nm -> Union.Member (Named nm e) effs =>
	Eff.E effs i o a -> (e -> Eff.E effs i o a) -> Eff.E effs i o a
catchN nm = (Eff.effh .) . Catch @nm

runN :: forall nm e effs i o a . HFunctor.Loose (Union.U effs) =>
	Eff.E (Named nm e ': effs) i o a -> Eff.E effs i o (Either e a)
runN = \case
	F.Pure x -> F.Pure $ Right x
	u F.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map runN Right u' F.:>>=
			Q.singleton (either (F.Pure . Left) (runN F.. q))
		Right (Throw e) -> F.Pure $ Left e
		Right (m `Catch` h) -> either (F.Pure . Left) (runN F.. q)
			=<< either (runN . h) (F.Pure . Right) =<< runN m

runExcN :: forall nm e e' effs i o a . forall nm' ->
	(HFunctor.Loose (Union.U effs), Union.Member (Named nm' e') effs) =>
	(e -> e') -> (e' -> e) ->
	Eff.E (Named nm e ': effs) i o a -> Eff.E effs i o a
runExcN nm' c c' = \case
	F.Pure x -> F.Pure x
	u F.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map ((Identity <$>) . runExcN nm' c c')
				Identity u' F.:>>=
			Q.singleton ((runExcN nm' c c' F.. q) . runIdentity)
		Right (Throw e) -> throwN nm' $ c e
		Right (m `Catch` h) -> runExcN nm' c c' F.. q =<< catchN nm'
			(runExcN nm' c c' m) (runExcN nm' c c' . h . c')

runION :: IO.Exception e =>
	Eff.E '[Named nm e, IO.I] i o a -> Eff.E '[IO.I] i o a
runION = \case
	F.Pure x -> F.Pure x
	u F.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map ((Identity <$>) . runION) Identity u'
			F.:>>= Q.singleton ((runION F.. q) . runIdentity)
		Right (Throw e) -> Eff.effBase $ IO.throwIO e
		Right (m `Catch` h) ->
			runION F.. q =<< runION m `cch` (runION . h)
	where m `cch` h = Eff.effBase $ Eff.runM m `IO.catch` (Eff.runM . h)
