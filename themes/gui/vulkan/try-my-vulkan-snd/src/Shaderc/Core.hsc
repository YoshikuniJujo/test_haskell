{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.Core where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Data.Word

import Shaderc.Options.Core

#include <shaderc/shaderc.h>

data ShadercCompilerTag
type ShadercCompilerT = Ptr ShadercCompilerTag

foreign import ccall "shaderc_compiler_initialize"
	c_shaderc_compiler_initialize :: IO ShadercCompilerT

foreign import ccall "shaderc_compiler_release"
	c_shaderc_compiler_release :: ShadercCompilerT -> IO ()

data ShadercCompilationResultTag
type ShadercCompilationResultT = Ptr ShadercCompilationResultTag

foreign import ccall "shaderc_compile_into_spv" c_shaderc_compile_into_spv ::
	ShadercCompilerT -> Ptr CChar -> #{type size_t} ->
	#{type shaderc_shader_kind} -> CString -> CString ->
	ShadercCompileOptionsT -> IO ShadercCompilationResultT

foreign import ccall "shaderc_compile_into_spv_assembly"
	c_shaderc_compile_into_spv_assembly ::
	ShadercCompilerT -> Ptr CChar -> #{type size_t} ->
	#{type shaderc_shader_kind} -> CString -> CString ->
	ShadercCompileOptionsT -> IO ShadercCompilationResultT

foreign import ccall "shaderc_compile_into_preprocessed_text"
	c_shaderc_compile_into_preprocessed_text ::
	ShadercCompilerT -> Ptr CChar -> #{type size_t} ->
	#{type shaderc_shader_kind} -> CString -> CString ->
	ShadercCompileOptionsT -> IO ShadercCompilationResultT

foreign import ccall "shaderc_result_release" c_shaderc_result_release ::
	ShadercCompilationResultT -> IO ()

foreign import ccall "shaderc_result_get_length" c_shaderc_result_get_length ::
	ShadercCompilationResultT -> IO #{type size_t}

foreign import ccall "shaderc_result_get_bytes" c_shaderc_result_get_bytes ::
	ShadercCompilationResultT -> IO (Ptr CChar)

foreign import ccall "shaderc_result_get_error_message"
	c_shaderc_result_get_error_message ::
	ShadercCompilationResultT -> IO CString
