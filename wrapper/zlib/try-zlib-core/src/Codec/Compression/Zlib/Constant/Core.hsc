{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Codec.Compression.Zlib.Constant.Core where

import Foreign.C.Enum
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

enum "CompressonLevel" ''#{type int} [''Show, ''Read, ''Eq] [
	("NoCompression", #{const Z_NO_COMPRESSION}),
	("BestSpeed", #{const Z_BEST_SPEED}),
	("BestCompression", #{const Z_BEST_COMPRESSION}),
	("DefaultCompression", #{const Z_DEFAULT_COMPRESSION}) ]

enum "CompressionMethod" ''#{type int} [''Show, ''Read, ''Eq] [
	("Deflated", #{const Z_DEFLATED}) ]
