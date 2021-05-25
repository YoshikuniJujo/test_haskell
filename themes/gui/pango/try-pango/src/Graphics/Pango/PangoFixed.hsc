module Graphics.Pango.PangoFixed where

import Foreign.C.Types
import Data.Fixed

#include <pango/pango.h>

data PU

instance HasResolution PU where resolution _ = #{const PANGO_SCALE}

type PangoFixed = Fixed PU

toPangoFixed :: CInt -> PangoFixed
toPangoFixed = MkFixed . fromIntegral

fromPangoFixed :: PangoFixed -> CInt
fromPangoFixed (MkFixed i) = fromIntegral i
