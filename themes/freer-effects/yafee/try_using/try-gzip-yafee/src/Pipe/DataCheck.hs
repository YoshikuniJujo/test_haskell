{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.DataCheck (checkRight) where

import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.Except qualified as Except
import Control.OpenUnion qualified as Union

checkRight :: forall effs . forall a -> forall b -> (
	Union.Member (Pipe.P (Either a b) b) effs,
	Union.Member (Except.E String) effs ) => Eff.E effs ()
checkRight a b = Pipe.await' @(Either a b) b >>= \case
	Just (Left _) -> Except.throw "(Left _) exist" >> checkRight a b
	Just (Right x) -> Pipe.yield' (Either a b) x >> checkRight a b
	Nothing -> pure ()
