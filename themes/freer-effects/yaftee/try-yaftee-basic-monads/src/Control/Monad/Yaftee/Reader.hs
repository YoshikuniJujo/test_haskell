{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Reader (

	-- * NORMAL

	R, ask, local, run,

	-- * NAMED

	Named, askN, localN, runN

	) where

import GHC.TypeLits
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.HigherFreer qualified as F
import Control.HigherOpenUnion qualified as Union
import Data.Kind
import Data.Functor.Identity
import Data.HigherFunctor qualified as HFunctor
import Data.FTCQueue qualified as Q

-- * NORMAL

type R e = Named "" e

ask :: Union.Member (R e) effs => Eff.E effs i o e
ask = askN ""

local :: forall e effs i o a . Union.Member (R e) effs =>
	(e -> e) -> Eff.E effs i o a -> Eff.E effs i o a
local = localN ""

run :: forall e effs i o a . HFunctor.Loose (Union.U effs) =>
	Eff.E (R e ': effs) i o a -> e -> Eff.E effs  i o a
run = runN

-- * NAMED

data Named (nm :: Symbol) e (f :: Type -> Type -> Type -> Type) i o a where
	Ask :: forall nm e f i o . Named nm e f i o e
	Local :: forall nm e f i o a . (e -> e) -> f i o a -> Named nm e f i o a

askN :: forall nm -> Union.Member (Named nm e) effs => Eff.E effs i o e
askN nm = Eff.effh (Ask @nm)

localN :: forall nm -> Union.Member (Named nm e) effs =>
	(e -> e) -> Eff.E effs i o a -> Eff.E effs i o a
localN nm = (Eff.effh .) . Local @nm

runN :: forall nm e effs i o a . HFunctor.Loose (Union.U effs) =>
	Eff.E (Named nm e ': effs) i o a -> e -> Eff.E effs i o a
m `runN` e = case m of
	F.Pure x -> F.Pure x
	u F.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.map ((Identity <$>) . (`runN` e)) Identity u' F.:>>=
			Q.singleton (((`runN` e) F.. q) . runIdentity)
		Right Ask -> (q F.$ e) `runN` e
		Right (Local f a) ->  (`runN` e) F.. q =<< a `runN` f e
