{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.Fail (F, pattern F, run, runIO, runExc, catch) where

import Control.Monad.Yafe.Eff qualified as Eff
import Control.Monad.Yafee.Except qualified as Except
import Control.OpenUnion qualified as Union

type F = Union.Fail

{-# COMPLETE F #-}

pattern F :: String -> F a
pattern F msg = Union.Fail msg

run :: Eff.E (F ': effs) a -> Eff.E effs (Either String a)
run = Eff.handleRelay (pure . Right) \(F msg) -> const . pure $ Left msg

runIO :: Union.Member IO effs => Eff.E (F ': effs) a -> Eff.E effs a
runIO = Eff.handleRelay pure \(F msg) -> const . Eff.eff $ fail @IO msg

runExc :: Union.Member (Except.E String) effs =>
	Eff.E (F ': effs) a -> Eff.E effs a
runExc = Eff.handleRelay pure \(F msg) -> const $ Except.throw msg

catch :: Union.Member F effs =>
	Eff.E effs a -> (String -> Eff.E effs a) -> Eff.E effs a
m `catch` h = Eff.interpose pure (\(F msg) -> const $ h msg) m
