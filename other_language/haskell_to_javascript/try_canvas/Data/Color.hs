{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Color where

import Data.Word
import Text.Printf

data Rgb = Rgb Word8 Word8 Word8 deriving Show

data Rgba = Rgba Word8 Word8 Word8 Word8 deriving Show

rgbToString :: Rgb -> String
rgbToString (Rgb r g b) = "#" ++ concatMap (printf "%02x") [r, g, b]

rgbaToString :: Rgba -> String
rgbaToString (Rgba r g b a) = "#" ++ concatMap (printf "%02x") [r, g, b, a]

newtype ColorName = ColorName String deriving Show

colorNameToString :: ColorName -> String
colorNameToString (ColorName nm) = nm

pattern Black :: ColorName
pattern Black <- ColorName "black" where Black = ColorName "black"

pattern White :: ColorName
pattern White <- ColorName "white" where White = ColorName "white"
