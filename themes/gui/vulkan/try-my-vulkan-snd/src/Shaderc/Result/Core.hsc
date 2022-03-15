{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.Result.Core where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Data.Word

import Shaderc.Core

#include <shaderc/shaderc.h>

foreign import ccall "shaderc_result_release" resultRelease ::
	CompilationResultT -> IO ()

foreign import ccall "shaderc_result_get_length" resultGetLength ::
	CompilationResultT -> IO #{type size_t}

foreign import ccall "shaderc_result_get_bytes" resultGetBytes ::
	CompilationResultT -> IO (Ptr CChar)

foreign import ccall "shaderc_result_get_error_message"
	resultGetErrorMessage ::
	CompilationResultT -> IO CString
