{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Compression.Zlib.Structure.Core (

	-- * STRUCTURE

	-- ** IMMUTABLE

	Stream, streamInitial,
	PAllocFunc, AllocFunc, PFreeFunc, FreeFunc, PtrBytef,

	streamNextIn, streamAvailIn, streamNextOut, streamAvailOut,
	streamAlloc, streamFree, streamOpaque,


	-- ** MUTABLE

	StreamIO, StreamST, StreamPrim,
	withStreamPtr, streamFreeze, streamThaw, streamCopy,

	-- * IN/OUT

	nextIn, availIn, nextOut, availOut, msg,
	setNextIn, setNextOut,

	) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Struct
import Foreign.C.Struct.TypeSynonyms
import Control.Monad.Primitive
import Data.Word
import Data.Int

-- import Codec.Compression.Zlib.Constant.Core

#include <zlib.h>

type AllocFunc = Ptr () -> #{type uInt} -> #{type uInt} -> IO (Ptr ())
type FreeFunc = Ptr () -> Ptr () -> IO ()

type PAllocFunc = FunPtr AllocFunc
type PFreeFunc = FunPtr FreeFunc

type PtrBytef = Ptr #{type Bytef}

struct "Stream" #{size z_stream} #{alignment z_stream}
	[	("nextIn", ''PtrBytef,
			[| #{peek z_stream, next_in} |],
			[| #{poke z_stream, next_in} |]),
		("availIn", ''#{type uInt},
			[| #{peek z_stream, avail_in} |],
			[| #{poke z_stream, avail_in} |]),
			{-
		("totalIn", ''#{type uLong},
			[| #{peek z_stream, total_in} |],
			[| #{poke z_stream, total_in} |]),
			-}

		("nextOut", ''PtrBytef,
			[| #{peek z_stream, next_out} |],
			[| #{poke z_stream, next_out} |]),
		("availOut", ''#{type uInt},
			[| #{peek z_stream, avail_out} |],
			[| #{poke z_stream, avail_out} |]),
			{-
		("totalOut", ''#{type uLong},
			[| #{peek z_stream, total_out} |],
			[| #{poke z_stream, total_out} |]),
			-}

{-
		("msg", ''CString,
			[| #{peek z_stream, msg} |],
			[| #{poke z_stream, msg} |]),
			-}

		("alloc", ''PAllocFunc,
			[| #{peek z_stream, zalloc} |],
			[| #{poke z_stream, zalloc} |]),
		("free", ''PFreeFunc,
			[| #{peek z_stream, zfree} |],
			[| #{poke z_stream, zfree} |]),
		("opaque", ''PtrVoid,
			[| #{peek z_stream, opaque} |],
			[| #{poke z_stream, opaque} |])

{-
		("dataType", ''DataType,
			[| #{peek z_stream, data_type} |],
			[| #{poke z_stream, data_type} |]),

		("adler", ''#{type uLong},
			[| #{peek z_stream, adler} |],
			[| #{poke z_stream, adler} |])
			-}
		]
	[''Show, ''Eq, ''Storable]

streamInitial :: Stream
streamInitial = Stream {
	streamNextIn = nullPtr,
	streamAvailIn = 0,
	streamNextOut = nullPtr,
	streamAvailOut = 0,
	streamAlloc = nullFunPtr,
	streamFree = nullFunPtr,
	streamOpaque = nullPtr }

streamCopyPtr :: Ptr Stream -> IO (Ptr Stream)
streamCopyPtr src = do
	dst <- malloc

	(ni :: PtrBytef) <- #{peek z_stream, next_in} src
	#{poke z_stream, next_in} dst ni
	(ai :: #{type uInt}) <- #{peek z_stream, avail_in} src
	#{poke z_stream, avail_in} dst ai
	(ti :: #{type uLong}) <- #{peek z_stream, total_in} src
	#{poke z_stream, total_in} dst ti

	(no :: PtrBytef) <- #{peek z_stream, next_out} src
	#{poke z_stream, next_out} dst no
	(ao :: #{type uInt}) <- #{peek z_stream, avail_out} src
	#{poke z_stream, avail_out} dst ao
	(to :: #{type uLong}) <- #{peek z_stream, total_out} src
	#{poke z_stream, total_out} dst to

	(msg :: CString) <- #{peek z_stream, msg} src
	#{poke z_stream, msg} dst msg
	(stt :: Ptr ()) <- #{peek z_stream, state} src
	#{poke z_stream, state} dst stt

	(allc :: PAllocFunc) <- #{peek z_stream, zalloc} src
	#{poke z_stream, zalloc} dst allc
	(fr :: PFreeFunc) <- #{peek z_stream, zfree} src
	#{poke z_stream, zfree} dst fr
	(opq :: Ptr ()) <- #{peek z_stream, opaque} src
	#{poke z_stream, opaque} dst opq

	(dt :: #{type int}) <- #{peek z_stream, data_type} src
	#{poke z_stream, data_type} dst dt

	(ad :: #{type uLong}) <- #{peek z_stream, adler} src
	#{poke z_stream, adler} dst ad
	(rs :: #{type uLong}) <- #{peek z_stream, reserved} src
	#{poke z_stream, reserved} dst rs

	pure dst

streamFreePtr :: Ptr Stream -> IO ()
streamFreePtr = free

structPrim "Stream" 'streamCopyPtr 'streamFreePtr [''Show]

withStreamPtr ::
	PrimBase m => StreamPrim (PrimState m) -> (Ptr Stream -> m a) -> m a
withStreamPtr (StreamPrim s) f =
	unsafeIOToPrim $ withForeignPtr s (unsafePrimToIO . f)

nextIn :: PrimMonad m => StreamPrim (PrimState m) -> m (Ptr Word8)
nextIn (StreamPrim s) = unsafeIOToPrim $ withForeignPtr s nextInPtr

availIn :: PrimMonad m => StreamPrim (PrimState m) -> m #{type uInt}
availIn (StreamPrim s) = unsafeIOToPrim $ withForeignPtr s availInPtr

nextInPtr :: Ptr Stream -> IO (Ptr Word8)
nextInPtr = #{peek z_stream, next_in}

availInPtr :: Ptr Stream -> IO #{type uInt}
availInPtr = #{peek z_stream, avail_in}

nextOut :: PrimMonad m => StreamPrim (PrimState m) -> m (Ptr Word8)
nextOut (StreamPrim s) = unsafeIOToPrim $ withForeignPtr s nextOutPtr

availOut :: PrimMonad m => StreamPrim (PrimState m) -> m #{type uInt}
availOut (StreamPrim s) = unsafeIOToPrim $ withForeignPtr s availOutPtr

nextOutPtr :: Ptr Stream -> IO (Ptr Word8)
nextOutPtr = #{peek z_stream, next_out}

availOutPtr :: Ptr Stream -> IO #{type uInt}
availOutPtr = #{peek z_stream, avail_out}

msg :: PrimMonad m => StreamPrim (PrimState m) -> m CString
msg (StreamPrim s) = unsafeIOToPrim $ withForeignPtr s msgPtr

msgPtr :: Ptr Stream -> IO CString
msgPtr = #{peek z_stream, msg}

setNextIn :: PrimMonad m =>
	StreamPrim (PrimState m) -> PtrBytef -> #{type uInt} -> m ()
setNextIn (StreamPrim s) ni ai = unsafeIOToPrim $ withForeignPtr s \p -> do
	#{poke z_stream, next_in} p ni
	#{poke z_stream, avail_in} p ai

setNextOut :: PrimMonad m =>
	StreamPrim (PrimState m) -> PtrBytef -> #{type uInt} -> m ()
setNextOut (StreamPrim s) no ao = unsafeIOToPrim $ withForeignPtr s \p -> do
	#{poke z_stream, next_out} p no
	#{poke z_stream, avail_out} p ao
