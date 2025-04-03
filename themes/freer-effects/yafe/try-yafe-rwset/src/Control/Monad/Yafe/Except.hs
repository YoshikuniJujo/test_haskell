{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafe.Except where

import Control.Monad.Yafe.Eff qualified as Eff
import Control.OpenUnion qualified as Union

data Exc e a = ThrowError e deriving Show

throwError :: Union.Member (Exc e) effs => e -> Eff.E effs a
throwError = Eff.eff . ThrowError

runError :: Eff.E (Exc e ': effs) a -> Eff.E effs (Either e a)
runError =
	Eff.handleRelay (pure . Right) \(ThrowError e) -> const . pure $ Left e

catchError :: Union.Member (Exc e) effs =>
	Eff.E effs a -> (e -> Eff.E effs a) -> Eff.E effs a
m `catchError` h = Eff.interpose pure (\(ThrowError e) -> const $ h e) m
