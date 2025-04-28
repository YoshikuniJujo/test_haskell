{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe.Gzip.Check where

import Control.Monad.Fix

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.Except qualified as Except
import Yaftee.OpenUnion qualified as Union

checkRight :: (
	Union.Member (Except.E String) effs,
	Union.Member Pipe.P effs ) =>
	Eff.E effs (Either a b) b r
checkRight = fix \go -> Pipe.await >>= (>> go)
	. either (const $ Except.throw "(Left _) exist") (Pipe.yield)
