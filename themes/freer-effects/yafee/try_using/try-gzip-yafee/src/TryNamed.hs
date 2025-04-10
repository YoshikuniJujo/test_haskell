{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module TryNamed where

import Control.Monad.Yafee.Eff qualified as Eff
import Control.Monad.Yafee.State qualified as State
import Control.OpenUnion qualified as Union

countUpDown :: (
	Union.Member (State.Named "count-up" Int) effs,
	Union.Member (State.Named "count-down" Int) effs
	) =>
	Eff.E effs ()
countUpDown = do
	State.modifyN @Int "count-up" (+ 1)
	State.modifyN @Int "count-down" (subtract 1)

foo :: (
	Union.Member (State.Named "count-up" Int) effs,
	Union.Member (State.Named "count-down" Int) effs
	) =>
	Eff.E effs ()
foo = do
	countUpDown
	countUpDown
	countUpDown
	countUpDown

fooRunned :: (((), Int), Int)
fooRunned = Eff.run
	. (flip (State.runN @"count-up") 0)
	$ (flip (State.runN @"count-down") 10) foo
