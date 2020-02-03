{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module ColorToPixel (color) where

import Field
import MonadicFrp

color :: Color -> Pixel
color Red = 0xff0000
color Green = 0x00ff00
color Blue = 0x0000ff
color Yellow = 0xffff00
color Cyan = 0x00ffff
color Magenta = 0xff00ff
