{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.DataCheck (checkRight) where

import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.Pipe qualified as Pipe
import Control.Monad.Yafee.Except qualified as Except
import Control.OpenUnion qualified as Union

checkRight :: Union.Member (Except.E String) effs =>
	Eff.E (Pipe.P (Either a b) b ': effs) ()
checkRight = Pipe.await >>= \case
	Just (Left _) -> Except.throw "(Left _) exist" >> checkRight
	Just (Right x) -> Pipe.yield x >> checkRight
	Nothing -> pure ()
