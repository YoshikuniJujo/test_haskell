{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Pipe.DataCheck (checkRight) where

import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.Except qualified as Except
import Control.HigherOpenUnion qualified as Union

checkRight :: Union.Member (Except.E String) effs =>
	Eff.E (Pipe.P (Either a b) b ': effs) ()
checkRight = Pipe.await >>= \case
	Left _ -> Except.throw "(Left _) exist" >> checkRight
	Right x -> Pipe.yield x >> checkRight
