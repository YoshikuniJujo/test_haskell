{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.UseFTCQ.Pipe.IO where

import Prelude hiding (print)
import Prelude qualified as P

import Control.Monad.Fix
import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.UseFTCQ.Pipe qualified as Pipe
import Yaftee.UseFTCQ.IO qualified as IO
import Yaftee.OpenUnion qualified as Union

print :: (Show i, Union.Member Pipe.P effs, Union.Base IO.I effs) =>
	Eff.E effs i o r
print = fix \go -> Pipe.await >>= (>> go) . Eff.effBase . P.print

print' :: (Show i, Union.Member Pipe.P effs, Union.Base IO.I effs) =>
	Eff.E effs i o ()
print' = fix \go -> do
	b <- Pipe.isMore
	if b
	then Pipe.await >>= (>> go) . Eff.effBase . P.print
	else pure ()
