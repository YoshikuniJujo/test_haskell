{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafe.Fail (
	Union.Fail(..), runFail, runFailIO, runFailExc, catchFail ) where

import Control.Monad.Yafe.Eff qualified as Eff
import Control.Monad.Yafe.Except qualified as Except
import Control.OpenUnion qualified as Union

runFail :: Eff.E (Union.Fail ': effs) a -> Eff.E effs (Either String a)
runFail =
	Eff.handleRelay (pure . Right) \(Union.Fail msg) -> const . pure $ Left msg

runFailIO :: Union.Member IO effs => Eff.E (Union.Fail ': effs) a -> Eff.E effs a
runFailIO =
	Eff.handleRelay pure \(Union.Fail msg) -> const . Eff.eff $ fail @IO msg

runFailExc :: Union.Member (Except.Exc String) effs => Eff.E (Union.Fail ': effs) a -> Eff.E effs a
runFailExc = Eff.handleRelay pure \(Union.Fail msg) -> const $ Except.throwError msg

catchFail :: Union.Member Union.Fail effs =>
	Eff.E effs a -> (String -> Eff.E effs a) -> Eff.E effs a
m `catchFail` h = Eff.interpose pure (\(Union.Fail msg) -> const $ h msg) m
