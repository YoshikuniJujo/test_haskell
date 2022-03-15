{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.Core where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Data.Word

#include <shaderc/shaderc.h>

data CompilerTag
type CompilerT = Ptr CompilerTag

data CompilationResultTag
type CompilationResultT = Ptr CompilationResultTag

data CompileOptionsTag
type CompileOptionsT = Ptr CompileOptionsTag

foreign import ccall "shaderc_compiler_initialize"
	compilerInitialize :: IO CompilerT

foreign import ccall "shaderc_compiler_release"
	compilerRelease :: CompilerT -> IO ()

foreign import ccall "shaderc_compile_into_spv" compileIntoSpv ::
	CompilerT -> Ptr CChar -> #{type size_t} ->
	#{type shaderc_shader_kind} -> CString -> CString ->
	CompileOptionsT -> IO CompilationResultT

foreign import ccall "shaderc_compile_into_spv_assembly"
	compileIntoSpvAssembly ::
	CompilerT -> Ptr CChar -> #{type size_t} ->
	#{type shaderc_shader_kind} -> CString -> CString ->
	CompileOptionsT -> IO CompilationResultT

foreign import ccall "shaderc_compile_into_preprocessed_text"
	compileIntoPreprocessedText ::
	CompilerT -> Ptr CChar -> #{type size_t} ->
	#{type shaderc_shader_kind} -> CString -> CString ->
	CompileOptionsT -> IO CompilationResultT

foreign import ccall "shaderc_result_release" resultRelease ::
	CompilationResultT -> IO ()

foreign import ccall "shaderc_result_get_length" resultGetLength ::
	CompilationResultT -> IO #{type size_t}

foreign import ccall "shaderc_result_get_bytes" resultGetBytes ::
	CompilationResultT -> IO (Ptr CChar)

foreign import ccall "shaderc_result_get_error_message"
	resultGetErrorMessage ::
	CompilationResultT -> IO CString
