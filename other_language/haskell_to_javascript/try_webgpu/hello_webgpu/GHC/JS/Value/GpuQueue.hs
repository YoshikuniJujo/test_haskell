{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuQueue where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object

import GHC.JS.Value.GpuBuffer qualified as JS.GpuBuffer
import GHC.JS.Value.Float32Array qualified as JS.Float32Array

newtype G = G { unG :: JSVal }

instance Show G where
	show g = "(" ++ JS.Object.toString (JS.Object.toO g) ++ ")"

instance JS.Value.IsJSVal G where toJSVal (G v) = v
instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue

instance JS.Object.IsO G

writeBuffer :: BufferData bd => G ->
	JS.GpuBuffer.G -> Int -> bd -> Int -> Int -> IO ()
writeBuffer (G g) (JS.GpuBuffer.G b) bo (JS.Value.toJSVal -> dt) dto sz =
	js_writeBuffer g b bo dt dto sz

class JS.Value.IsJSVal bd => BufferData bd

instance BufferData JS.Float32Array.F

foreign import javascript
	"((g, b, bo, dt, dto, sz) => { g.writeBuffer(b, bo, dt, dto, sz) })"
	js_writeBuffer :: JSVal -> JSVal -> Int -> JSVal -> Int -> Int -> IO ()
