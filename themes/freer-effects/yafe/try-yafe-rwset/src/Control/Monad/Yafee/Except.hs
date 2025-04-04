{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.Except (E(..), throw, run, catch) where

import Control.Monad.Yafe.Eff qualified as Eff
import Control.OpenUnion qualified as Union

data E e a = Throw e deriving Show

throw :: Union.Member (E e) effs => e -> Eff.E effs a
throw = Eff.eff . Throw

run :: Eff.E (E e ': effs) a -> Eff.E effs (Either e a)
run = Eff.handleRelay (pure . Right) \(Throw e) -> const . pure $ Left e

catch :: Union.Member (E e) effs =>
	Eff.E effs a -> (e -> Eff.E effs a) -> Eff.E effs a
m `catch` h = Eff.interpose pure (\(Throw e) -> const $ h e) m
