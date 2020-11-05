{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Monad where

import GHC.Base
import Control.Monad.Primitive

unPrimIo :: PrimMonad m => IO a -> m a
unPrimIo = primitive . unsafeCoerce# . unIO
