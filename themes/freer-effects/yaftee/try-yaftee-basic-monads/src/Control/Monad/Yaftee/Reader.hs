{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
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
import Control.HigherOpenUnion qualified as Union
import Data.Functor.Identity
import Data.HFunctor qualified as HFunctor

-- * NORMAL

type R e = Named "" e

ask :: Union.Member (R e) effs => Eff.E effs e
ask = askN ""

local ::  forall e effs a . Union.Member (R e) effs =>
	(e -> e) -> Eff.E effs a -> Eff.E effs a
local = localN ""

run :: forall e effs a .
	HFunctor.H (Union.U effs) => Eff.E (R e ': effs) a -> e -> Eff.E effs a
run = runN

-- * NAMED

type Named nm e = Union.FromFirst (Named_ nm e)
data Named_ (nm :: Symbol) e a where AskN :: Named_ nm e e

askN :: forall nm -> Union.Member (Named nm e) effs => Eff.E effs e
askN nm = Eff.eff (AskN @nm)

localN :: forall e effs a . forall nm -> Union.Member (Named nm e) effs =>
	(e -> e) -> Eff.E effs a -> Eff.E effs a
localN nm f m = do
	e <- f <$> askN nm
	let	h :: Named_ nm e v -> (v -> Eff.E effs a) -> Eff.E effs a
		h AskN k = k e
	Eff.interpose pure h m

runN :: forall nm e effs a .
	HFunctor.H (Union.U effs) =>
	Eff.E (Named nm e ': effs) a -> e -> Eff.E effs a
m `runN` e =
	runIdentity <$> Eff.handleRelay Identity runIdentity (\AskN k -> k e) m
