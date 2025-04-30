{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Reader (

	-- * NORMAL

	R, ask, -- local,
--	run,

	-- * NAMED

	Named, askN, -- localN,
	runN

	) where

import GHC.TypeLits
import Control.Monad.Fix
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.HFreer qualified as HFreer
import Yaftee.OpenUnion qualified as Union
import Data.Functor.Identity
import Data.FTCQueue qualified as Q
import Yaftee.HFunctor qualified as HFunctor

-- * NORMAL

type R e = Named "" e

ask :: Union.Member (R e) effs => Eff.E effs i o e
ask = askN ""

{-
local ::  forall e effs a . Union.Member (R e) effs =>
	(e -> e) -> Eff.E effs a -> Eff.E effs a
local = localN ""

run :: forall e effs i o a .
	HFunctor.HFunctor (Union.U effs) => Eff.E (R e ': effs) i o a -> e -> Eff.E effs i o a
run = runN
-}

-- * NAMED

type Named nm e = Union.FromFirst (Named_ nm e)
data Named_ (nm :: Symbol) e a where Ask :: Named_ nm e e

askN :: forall nm -> Union.Member (Named nm e) effs => Eff.E effs i o e
askN nm = Eff.eff (Ask @nm)

{-
localN :: forall e effs a . forall nm -> Union.Member (Named nm e) effs =>
	(e -> e) -> Eff.E effs a -> Eff.E effs a
localN nm f m = do
	e <- f <$> askN nm
	let	h :: Named_ nm e v -> (v -> Eff.E effs a) -> Eff.E effs a
		h Ask k = k e
	Eff.interpose pure h m
	-}

runN :: forall nm e effs i o a . (
	HFunctor.HFunctor' (Union.U effs),
	HFunctor.HFunctorSimple (Union.U effs)
	) =>
	Eff.E (Named nm e ': effs) i o a -> e -> Eff.E effs i o a
m `runN` e = ($ m) $ fix \go -> \case
	HFreer.Pure x -> HFreer.Pure x
	u HFreer.:>>= q -> case Union.decomp u of
		Left u' -> HFunctor.hmapS (`runN` e) Eff.weaken u' HFreer.:>>=
			Q.singleton \x -> go $ q `HFreer.app` x
		Right (Union.FromFirst Ask k) -> (\x' -> (go `HFreer.comp` q) $ k x') e

	{-
m `runN` e =
	runIdentity <$> Eff.handleRelay Identity runIdentity (\Ask k -> k e) m
	-}
