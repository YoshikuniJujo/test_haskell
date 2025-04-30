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
import Control.HigherOpenUnion qualified as Union
import Data.Functor.Identity
import Data.HigherFunctor qualified as HFunctor

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

type Named nm e = Union.FromFirst (Named_ nm e)
data Named_ (nm :: Symbol) e a where Ask :: Named_ nm e e

askN :: forall nm -> Union.Member (Named nm e) effs => Eff.E effs i o e
askN nm = Eff.eff (Ask @nm)

localN :: forall e effs i o a . forall nm -> Union.Member (Named nm e) effs =>
	(e -> e) -> Eff.E effs i o a -> Eff.E effs i o a
localN nm f m = do
	e <- f <$> askN nm
	let	h :: Named_ nm e v ->
			(v -> Eff.E effs i o a) -> Eff.E effs i o a
		h Ask k = k e
	Eff.interpose pure h m

runN :: forall nm e effs i o a . HFunctor.Loose (Union.U effs) =>
	Eff.E (Named nm e ': effs) i o a -> e -> Eff.E effs i o a
m `runN` e =
	runIdentity <$> Eff.handleRelay Identity runIdentity (\Ask k -> k e) m
