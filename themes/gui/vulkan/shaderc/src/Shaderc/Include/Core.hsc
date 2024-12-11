{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.Include.Core where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String
import Foreign.C.Enum
import Foreign.C.Struct
import Data.Word

#include <shaderc/shaderc.h>

type PtrVoid = Ptr ()

struct "Result" #{size shaderc_include_result}
		#{alignment shaderc_include_result} [
	("sourceName", ''CString,
		[| #{peek shaderc_include_result, source_name} |],
		[| #{poke shaderc_include_result, source_name} |]),
	("sourceNameLength", ''#{type size_t},
		[| #{peek shaderc_include_result, source_name_length} |],
		[| #{poke shaderc_include_result, source_name_length} |]),
	("content", ''CString,
		[| #{peek shaderc_include_result, content} |],
		[| #{poke shaderc_include_result, content} |]),
	("contentLength", ''#{type size_t},
		[| #{peek shaderc_include_result, content_length} |],
		[| #{poke shaderc_include_result, content_length} |]),
	("userData", ''PtrVoid,
		[| #{peek shaderc_include_result, user_data} |],
		[| #{poke shaderc_include_result, user_data} |]) ]
	[''Show, ''Storable]

enum "Type" ''#{type enum shaderc_include_type} [''Show, ''Storable] [
	("TypeRelative", #{const shaderc_include_type_relative}),
	("TypeStandard", #{const shaderc_include_type_standard}) ]

type ResolveFn =
	PtrVoid -> CString -> Type -> CString -> #{type size_t} ->
	IO (Ptr Result)

foreign import ccall "wrapper"
	wrap_resolveFn :: ResolveFn -> IO (FunPtr ResolveFn)

type ResultReleaseFn = PtrVoid -> Ptr Result -> IO ()

foreign import ccall "wrapper"
	wrap_resultReleaseFn :: ResultReleaseFn -> IO (FunPtr ResultReleaseFn)
