{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.Result.Core where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Data.Word

import Shaderc.Core

#include <shaderc/shaderc.h>

foreign import ccall "shaderc_result_release"
	release :: CompilationResultT -> IO ()

foreign import ccall "shaderc_result_get_length"
	getLength :: CompilationResultT -> IO #{type size_t}

foreign import ccall "shaderc_result_get_num_warnings"
	getNumWarnings :: CompilationResultT -> IO #{type size_t}

foreign import ccall "shaderc_result_get_num_errors"
	getNumErrors :: CompilationResultT -> IO #{type size_t}

foreign import ccall "shaderc_result_get_compilation_status"
	getCompilationStatus :: CompilationResultT -> IO CompilationStatus

foreign import ccall "shaderc_result_get_bytes"
	getBytes :: CompilationResultT -> IO (Ptr CChar)

foreign import ccall "shaderc_result_get_error_message"
	getErrorMessage :: CompilationResultT -> IO CString
