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
	compilerInitialize :: IO ShadercCompilerT

foreign import ccall "shaderc_compiler_release"
	compilerRelease :: ShadercCompilerT -> IO ()

data ShadercCompilationResultTag
type ShadercCompilationResultT = Ptr ShadercCompilationResultTag

foreign import ccall "shaderc_compile_into_spv" compileIntoSpv ::
	ShadercCompilerT -> Ptr CChar -> #{type size_t} ->
	#{type shaderc_shader_kind} -> CString -> CString ->
	ShadercCompileOptionsT -> IO ShadercCompilationResultT

foreign import ccall "shaderc_compile_into_spv_assembly"
	compileIntoSpvAssembly ::
	ShadercCompilerT -> Ptr CChar -> #{type size_t} ->
	#{type shaderc_shader_kind} -> CString -> CString ->
	ShadercCompileOptionsT -> IO ShadercCompilationResultT

foreign import ccall "shaderc_compile_into_preprocessed_text"
	compileIntoPreprocessedText ::
	ShadercCompilerT -> Ptr CChar -> #{type size_t} ->
	#{type shaderc_shader_kind} -> CString -> CString ->
	ShadercCompileOptionsT -> IO ShadercCompilationResultT

foreign import ccall "shaderc_result_release" resultRelease ::
	ShadercCompilationResultT -> IO ()

foreign import ccall "shaderc_result_get_length" resultGetLength ::
	ShadercCompilationResultT -> IO #{type size_t}

foreign import ccall "shaderc_result_get_bytes" resultGetBytes ::
	ShadercCompilationResultT -> IO (Ptr CChar)

foreign import ccall "shaderc_result_get_error_message"
	resultGetErrorMessage ::
	ShadercCompilationResultT -> IO CString
