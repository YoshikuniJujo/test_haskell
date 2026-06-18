module GHC.JS.Value.GpuBufferUsage where

import System.IO.Unsafe
import Data.Maybe

import GHC.JS.Prim(JSVal)
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
