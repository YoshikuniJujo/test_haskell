{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.Yaftee.Pipe.IO where

import Prelude hiding (putChar, putStr, putStrLn, print, getChar)
import Prelude qualified as P

import Control.Monad.Fix
import Control.Monad.Yaftee.Eff qualified as Eff
import Control.Monad.Yaftee.Pipe qualified as Pipe
import Control.Monad.Yaftee.IO qualified as IO
import Control.HigherOpenUnion qualified as U

print :: (Show a, U.Base IO.I es) => Eff.E (Pipe.P a o ': es) ()
print = fix \go -> Pipe.await >>= (>> go) . Eff.effBase . P.print
