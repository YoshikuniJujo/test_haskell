{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuDevice where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.EventTarget qualified as JS.EventTarget

import GHC.JS.Value.GpuBuffer qualified as JS.GpuBuffer
import GHC.JS.Value.GpuQueue qualified as JS.GpuQueue
import GHC.JS.Value.GpuBufferUsage qualified as JS.GpuBufferUsage

newtype G = G JSVal

instance Show G where
	show g = "(" ++ JS.Object.toString (JS.Object.toO g) ++ ")"

instance JS.Value.IsJSVal G where toJSVal (G v) = v

instance JS.Value.V G where
	toV = JS.EventTarget.toValue; fromV = JS.EventTarget.fromValue

instance JS.Object.IsO G
instance JS.EventTarget.IsE G

createBuffer :: G -> BufferDescriptor -> IO JS.GpuBuffer.G
createBuffer g d = createBuffer' g =<< bufferDescriptorToObject d

createBuffer' :: G -> JS.Object.O -> IO JS.GpuBuffer.G
createBuffer' (G g) (JS.Value.toJSVal -> d) =
	JS.GpuBuffer.G <$> js_createBuffer g d

foreign import javascript "((g, d) => { return g.createBuffer(d); })"
	js_createBuffer :: JSVal -> JSVal -> IO JSVal

bufferDescriptorToObject :: BufferDescriptor -> IO JS.Object.O
bufferDescriptorToObject d = do
	o <- JS.Object.new @JS.Object.IO
	maybe (pure ()) (JS.Object.set o "label")
		$ bufferDescriptorLabel d
	maybe (pure ()) (JS.Object.set o "mappedAtCreation")
		$ bufferDescriptorMappedAtCreation d
	JS.Object.set o "size" $ bufferDescriptorSize d
	JS.Object.set o "usage" $ bufferDescriptorUsage d
	JS.Object.freeze o

data BufferDescriptor = BufferDescriptor {
	bufferDescriptorLabel :: Maybe String,
	bufferDescriptorMappedAtCreation :: Maybe Bool,
	bufferDescriptorSize :: Int,
	bufferDescriptorUsage :: JS.GpuBufferUsage.G }
	deriving Show

bufferDescriptor :: Int -> JS.GpuBufferUsage.G -> BufferDescriptor
bufferDescriptor s u = BufferDescriptor {
	bufferDescriptorLabel = Nothing,
	bufferDescriptorMappedAtCreation = Nothing,
	bufferDescriptorSize = s,
	bufferDescriptorUsage = u }

foreign import javascript "(() => { return false })" js_false :: JSVal
foreign import javascript "(() => { return true })" js_true :: JSVal

queue :: G -> JS.GpuQueue.G
queue (G g) = JS.GpuQueue.G $ js_queue g

foreign import javascript "((g) => { return g.queue; })"
	js_queue :: JSVal -> JSVal
