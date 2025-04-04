{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.Reader where

import Control.Monad.Yafee.Eff qualified as Eff
import Control.OpenUnion qualified as Union

data Reader e a where Ask :: Reader e e

ask :: Union.Member (Reader e) effs => Eff.E effs e
ask = Eff.eff Ask

local :: forall e effs a .
	Union.Member (Reader e) effs => (e -> e) -> Eff.E effs a -> Eff.E effs a
local f m = do
	e <- f <$> ask
	let	h :: Reader e v -> (v -> Eff.E effs a) -> Eff.E effs a
		h Ask k = k e
	Eff.interpose pure h m

runReader :: Eff.E (Reader e ': effs) a -> e -> Eff.E effs a
m `runReader` e = Eff.handleRelay pure (\Ask k -> k e) m
