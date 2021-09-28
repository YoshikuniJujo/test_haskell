{-# LANGUAGE GADTs, StandaloneDeriving #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foo where

import Foreign.C.Types
import Control.Monad.Primitive
import Data.Color
import Data.CairoContext
import Graphics.Cairo.Drawing.CairoT

cairoDrawSource :: CairoT r RealWorld -> Rgba CDouble -> IO ()
-- cairoDrawSource :: PrimMonad m => CairoT r (PrimState m) -> Rgba CDouble -> m ()
cairoDrawSource = cairoSetSourceRgba
