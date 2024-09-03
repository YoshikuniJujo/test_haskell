{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module KeyToXKey (keyToXKey) where

import Data.KeySym
import Graphics.UI.GlfwG.Key qualified as GlfwG.Ky

import KeyToXKey.TH

sequence [mkKeyToXKeyType, mkKeyToXKey]
