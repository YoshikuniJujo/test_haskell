{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Monad where

import Control.Monad.Primitive
import GHC.Base

unPrimIo :: PrimMonad m => IO a -> m a
unPrimIo = primitive . unsafeCoerce# . unIO

primIo :: PrimBase m => m a -> IO a
primIo = IO . unsafeCoerce# . internal
