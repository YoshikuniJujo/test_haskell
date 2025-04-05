{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.Reader (R(..), ask, local, run) where

import Control.Monad.Yafee.Eff qualified as Eff
import Control.OpenUnion qualified as Union

data R e a where Ask :: R e e

ask :: Union.Member (R e) effs => Eff.E effs e
ask = Eff.eff Ask

local :: forall e effs a .
	Union.Member (R e) effs => (e -> e) -> Eff.E effs a -> Eff.E effs a
local f m = do
	e <- f <$> ask
	let	h :: R e v -> (v -> Eff.E effs a) -> Eff.E effs a
		h Ask k = k e
	Eff.interpose pure h m

run :: Eff.E (R e ': effs) a -> e -> Eff.E effs a
m `run` e = Eff.handleRelay pure (\Ask k -> k e) m
