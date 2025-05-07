{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.Samples where

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.State qualified as State
import Yaftee.UseFTCQ.Except qualified as Except
import Yaftee.OpenUnion qualified as Union

transact :: (
	Union.Member (State.S Integer) effs,
	Union.Member (Except.E String) effs
	) =>
	Eff.E effs i o Integer
transact = do
	State.put 1
	(State.put 2 >> Except.throw "foobar") `Except.catch` (\(_ :: String) -> pure ())
	State.get
