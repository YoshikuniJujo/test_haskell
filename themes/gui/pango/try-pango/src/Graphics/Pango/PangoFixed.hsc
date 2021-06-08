{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.PangoFixed (PangoFixed, PU, fromCInt, toCInt) where

import GHC.Stack
import Foreign.C.Types
import Control.Exception
import Data.Fixed
import System.IO.Unsafe

#include <pango/pango.h>

data PU

instance HasResolution PU where resolution _ = #{const PANGO_SCALE}

type PangoFixed = Fixed PU

minCInt, maxCInt :: Integer
minCInt = fromIntegral (minBound @CInt)
maxCInt = fromIntegral (maxBound @CInt)

fromCInt :: CInt -> PangoFixed
fromCInt = MkFixed . fromIntegral

toCInt :: HasCallStack => PangoFixed -> CInt
toCInt (MkFixed i)
	| minCInt <= i && i <= maxCInt = fromIntegral i
	| otherwise = unsafePerformIO do
		putStrLn $ prettyCallStack callStack
		throw Overflow
