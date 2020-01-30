{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module StdMonadicFrp where

import Data.Set
import MonadicFrp

tryStd :: IO Int
tryStd = interpretSig morr print cycleColor

morr :: EvReqs GUIEv -> IO (EvOccs GUIEv)
morr r = getLine >>= \case
	"middle" -> pure . singleton . MouseDown $ Occurred [MMiddle]
	"right" -> pure . singleton . MouseDown $ Occurred [MRight]
	_ -> pure . singleton . MouseDown $ Occurred [MMiddle]
