{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.CompilationResult.Core where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Data.Word

import Language.SpirV.Shaderc.Exception.Enum

#include <shaderc/shaderc.h>

data Tag
type T = Ptr Tag

foreign import ccall "shaderc_result_release" release :: T -> IO ()

foreign import ccall "shaderc_result_get_length"
	getLength :: T -> IO #{type size_t}

foreign import ccall "shaderc_result_get_num_warnings"
	getNumWarnings :: T -> IO #{type size_t}

foreign import ccall "shaderc_result_get_num_errors"
	getNumErrors :: T -> IO #{type size_t}

foreign import ccall "shaderc_result_get_compilation_status"
	getCompilationStatus :: T -> IO CompilationStatus

foreign import ccall "shaderc_result_get_bytes" getBytes :: T -> IO (Ptr CChar)

foreign import ccall "shaderc_result_get_error_message"
	getErrorMessage :: T -> IO CString
