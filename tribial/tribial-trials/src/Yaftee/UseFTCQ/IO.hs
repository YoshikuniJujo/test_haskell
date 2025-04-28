{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE FlexibleContexts #-}

module Yaftee.UseFTCQ.IO where

import Prelude hiding (print)
import Prelude qualified as P

import Yaftee.UseFTCQ.Eff qualified as Eff
import Yaftee.OpenUnion qualified as Union

type I = Union.FromFirst IO

print :: (Show a, Union.Base I effs) => a -> Eff.E effs i o ()
print = Eff.effBase . P.print
