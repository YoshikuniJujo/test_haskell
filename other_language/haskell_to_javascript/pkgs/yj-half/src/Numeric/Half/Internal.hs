{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Numeric.Half.Internal where

import GHC.Generics
import GHC.JS.Prim(JSVal)

import Foreign.C.Types

newtype Half = Half { getHalf :: CUShort } deriving Generic

instance Show Half where showsPrec d h = showsPrec d (fromHalf h)

foreign import javascript "((n) => { return Math.f16round(n); }"
	js_f16round :: JSVal -> JSVal

foreign import javascript "hs_halfToFloat" fromHalf :: Half -> Float
