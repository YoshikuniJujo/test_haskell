{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Yaftee.Pipe.IO where

import Prelude hiding (print)
import Prelude qualified as P

import Control.Monad.Fix
import Yaftee.Eff qualified as Eff
import Yaftee.NewPipe qualified as Pipe
import Yaftee.OpenUnion qualified as Union
import Yaftee.TypeElem qualified as Elem

type I = Union.FromFirst IO

print :: (Elem.Member Pipe.Await effs, Elem.Base I effs, Show a) =>
	Eff.E effs a o ()
print = fix \go -> Pipe.await >>= (>> go) . Eff.effBase . P.print
