{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.Legacy.Pipe.IO where

import Prelude hiding (print)
import Prelude qualified as P

import Control.Monad.Fix
import Yaftee.Eff qualified as Eff
import Yaftee.Legacy.NewPipe qualified as Pipe
import Yaftee.IO qualified as IO
import Yaftee.TypeElem qualified as Elem

print :: (Elem.Member Pipe.Await effs, Elem.Base IO.I effs, Show a) =>
	Eff.E effs a o ()
print = fix \go -> Pipe.await >>= (>> go) . Eff.effBase . P.print
