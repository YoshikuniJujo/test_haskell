{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.Pipe.Tools where

import Prelude hiding (take)
import Control.Monad.Fix
import Yaftee.Eff qualified as Eff
import Yaftee.Pipe qualified as Pipe
import Yaftee.OpenUnion qualified as Union

convert :: Union.Member Pipe.P effs => (i -> o) -> Eff.E effs i o r
convert f = fix \go -> (Pipe.yield . f =<< Pipe.await) >> go

take :: Union.Member Pipe.P effs => Int -> Eff.E effs a a ()
take 0 = pure ()
take n = (Pipe.yield =<< Pipe.await) >> take (n - 1)
