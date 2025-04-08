{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yafee.Except (

	E(..), throw, run, runExc, runFail, runIO, catch

	) where

import Control.Monad.Yafee.Eff qualified as Eff
import Control.OpenUnion qualified as Union
import Control.Exception qualified as IO

data E e a = Throw e deriving Show

throw :: Union.Member (E e) effs => e -> Eff.E effs a
throw = Eff.eff . Throw

run :: Eff.E (E e ': effs) a -> Eff.E effs (Either e a)
run = Eff.handleRelay (pure . Right) \(Throw e) -> const . pure $ Left e

runIO :: (
	IO.Exception e,
	Union.Member IO effs ) => Eff.E (E e ': effs) a -> Eff.E effs a
runIO = Eff.handleRelay pure \(Throw e) f -> Eff.eff (IO.throwIO e) >>= f

runExc :: (
	Union.Member (E e') effs
	) =>
	(e -> e') -> Eff.E (E e ': effs) a -> Eff.E effs a
runExc c = Eff.handleRelay pure \(Throw e) f -> throw (c e) >>= f

runFail :: (
	Union.Member Union.Fail effs
	) =>
	Eff.E (E String ': effs) a -> Eff.E effs a
runFail = Eff.handleRelay pure \(Throw e) f -> fail e >>= f

catch :: Union.Member (E e) effs =>
	Eff.E effs a -> (e -> Eff.E effs a) -> Eff.E effs a
m `catch` h = Eff.interpose pure (\(Throw e) -> const $ h e) m
