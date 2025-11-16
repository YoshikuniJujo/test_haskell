{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Compression.Zlib.Constant.Core (

	-- * RETURN CODE

	ReturnCode(..),
	pattern Ok,
	pattern StreamEnd,
	pattern NeedDict,
	pattern Errno,
	pattern StreamError,
	pattern DataError,
	pattern MemError,
	pattern BufError,
	pattern VersionError,

	-- * FLUSH

	Flush(..),
	pattern NoFlush,
	pattern PartialFlush,
	pattern SyncFlush,
	pattern FullFlush,
	pattern Finish,
	pattern Block,
	pattern Trees,


	-- * MEM LEVEL

	MemLevel(..),

	-- * COMPRESSION STRATEGY

	CompressionStrategy(..),
	pattern Filtered,
	pattern HuffmanOnly,
	pattern Rle,
	pattern Fixed,
	pattern DefaultStrategy,

	-- * COMPRESSION LEVEL

	CompressionLevel(..),
	pattern NoCompression,
	pattern BestSpeed,
	pattern BestCompression,
	pattern DefaultCompression,

	-- * COMPRESSION METHOD

	CompressionMethod(..),
	pattern Deflated

	) where

import Foreign.Storable
import Foreign.C.Enum
import Control.Exception.Hierarchy
import Data.Int

#include <zlib.h>

enum "Flush" ''#{type int} [''Show, ''Read, ''Eq] [
	("NoFlush", #{const Z_NO_FLUSH}),
	("PartialFlush", #{const Z_PARTIAL_FLUSH}),
	("SyncFlush", #{const Z_SYNC_FLUSH}),
	("FullFlush", #{const Z_FULL_FLUSH}),
	("Finish", #{const Z_FINISH}),
	("Block", #{const Z_BLOCK}),
	("Trees", #{const Z_TREES}) ]

enum "ReturnCode" ''#{type int} [''Show, ''Read, ''Eq] [
	("Ok", #{const Z_OK}),
	("StreamEnd", #{const Z_STREAM_END}),
	("NeedDict", #{const Z_NEED_DICT}),
	("Errno", #{const Z_ERRNO}),
	("StreamError", #{const Z_STREAM_ERROR}),
	("DataError", #{const Z_DATA_ERROR}),
	("MemError", #{const Z_MEM_ERROR}),
	("BufError", #{const Z_BUF_ERROR}),
	("VersionError", #{const Z_VERSION_ERROR}) ]

exceptionHierarchy Nothing (ExType ''ReturnCode)

enum "CompressionLevel" ''#{type int} [''Show, ''Read, ''Eq] [
	("NoCompression", #{const Z_NO_COMPRESSION}),
	("BestSpeed", #{const Z_BEST_SPEED}),
	("BestCompression", #{const Z_BEST_COMPRESSION}),
	("DefaultCompression", #{const Z_DEFAULT_COMPRESSION}) ]

enum "CompressionStrategy" ''#{type int} [''Show, ''Read, ''Eq] [
	("Filtered", #{const Z_FILTERED}),
	("HuffmanOnly", #{const Z_HUFFMAN_ONLY}),
	("Rle", #{const Z_RLE}),
	("Fixed", #{const Z_FIXED}),
	("DefaultStrategy", #{const Z_DEFAULT_STRATEGY}) ]

enum "DataType" ''#{type int} [''Show, ''Read, ''Eq, ''Storable] [
	("Binary", #{const Z_BINARY}),
	("Text", #{const Z_TEXT}),
	("Ascii", #{const Z_ASCII}),
	("Unknown", #{const Z_UNKNOWN}) ]

enum "CompressionMethod" ''#{type int} [''Show, ''Read, ''Eq] [
	("Deflated", #{const Z_DEFLATED}) ]

enum "MemLevel" ''#{type int} [''Show, ''Read, ''Eq] [
	]
