{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.Pipe.IO where

import Prelude hiding (print)
import Prelude qualified as P

import Control.Monad.Fix
import Yaftee.Eff qualified as Eff
import Yaftee.Pipe qualified as Pipe
import Yaftee.IO qualified as IO
import Yaftee.OpenUnion qualified as Union

print :: (Show i, Union.Member Pipe.P effs, Union.Base IO.I effs ) =>
	Eff.E effs i o a
print = fix \go -> Pipe.await >>= (>> go) . Eff.effBase . P.print
