{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuBufferUsage where

import System.IO.Unsafe
import Data.Bits
import Data.Maybe

import GHC.JS.Prim(JSVal, toJSInt)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object

gpuBufferUsage :: JS.Object.O
gpuBufferUsage = JS.Object.otherO js_gpuBufferUsage

foreign import javascript "(() => { return GPUBufferUsage })"
	js_gpuBufferUsage :: JSVal

mkFlag :: String -> Int
mkFlag nm = unsafePerformIO
	$ fromJust <$> JS.Object.getInt gpuBufferUsage nm

copySrc, copyDst, index, indirect,
	mapRead, mapWrite, queryResolve, storage, uniform, vertex :: Int
copySrc = mkFlag "COPY_SRC"
copyDst = mkFlag "COPY_DST"
index = mkFlag "INDEX"
indirect = mkFlag "INDIRECT"
mapRead = mkFlag "MAP_READ"
mapWrite = mkFlag "MAP_WRITE"
queryResolve = mkFlag "QUERY_RESOLVE"
storage = mkFlag "STORAGE"
uniform = mkFlag "UNIFORM"
vertex = mkFlag "VERTEX"

newtype G = G Int deriving (Eq, Bits)

pattern CopySrc, CopyDst, Index, Indirect, MapRead, MapWrite,
	QueryResolve, Storage, Uniform, Vertex :: G
pattern CopySrc = G 0x0004
pattern CopyDst = G 0x0008
pattern Index = G 0x0010
pattern Indirect = G 0x0100
pattern MapRead = G 0x0001
pattern MapWrite = G 0x0002
pattern QueryResolve = G 0x0200
pattern Storage = G 0x0080
pattern Uniform = G 0x0040
pattern Vertex = G 0x0020

instance Show G where
	show = \case
		CopySrc -> "CopySrc"; CopyDst -> "CopyDst"
		Index -> "Index"; Indirect -> "Indirect"
		MapRead -> "MapRead"; MapWrite -> "MapWrite"
		QueryResolve -> "QueryResolve";
		Storage -> "Storage"; Uniform -> "Uniform"; Vertex -> "Vertex"
		G g -> "(G " ++ show g ++ ")"

toInt :: G -> Int
toInt (G g) = g

instance JS.Value.IsJSVal G where toJSVal = toJSInt . toInt
instance JS.Value.V G
