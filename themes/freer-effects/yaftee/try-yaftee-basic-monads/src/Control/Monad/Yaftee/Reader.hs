{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Reader where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.HigherOpenUnion qualified as Union
import Data.Functor.Identity
import Data.HFunctor qualified as HFunctor

type R e = Union.FromFirst (R_ e)
data R_ e a where Ask :: R_ e e

ask :: Union.Member (R e) effs => Eff.E effs e
ask = Eff.eff Ask

local ::  forall e effs a . Union.Member (R e) effs =>
	(e -> e) -> Eff.E effs a -> Eff.E effs a
local f m = do
	e <- f <$> ask
	let	h :: R_ e v -> (v -> Eff.E effs a) -> Eff.E effs a
		h Ask k = k e
	Eff.interpose pure h m

run :: HFunctor.H (Union.U effs) =>
	Eff.E (R e ': effs) a -> e -> Eff.E effs a
m `run` e =
	runIdentity <$> Eff.handleRelay Identity runIdentity (\Ask k -> k e) m
