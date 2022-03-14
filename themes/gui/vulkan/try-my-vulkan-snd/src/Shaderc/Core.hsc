{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.Core where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Data.Word
import Data.Int

#include <shaderc/shaderc.h>

data ShadercCompilerTag
type ShadercCompilerT = Ptr ShadercCompilerTag

foreign import ccall "shaderc_compiler_initialize"
	c_shaderc_compiler_initialize :: IO ShadercCompilerT

foreign import ccall "shaderc_compiler_release"
	c_shaderc_compiler_release :: ShadercCompilerT -> IO ()

data ShadercCompileOptionsTag
type ShadercCompileOptionsT = Ptr ShadercCompileOptionsTag

foreign import ccall "shaderc_compile_options_initialize"
	c_shaderc_compile_options_initialize :: IO ShadercCompileOptionsT

foreign import ccall "shaderc_compile_options_clone"
	c_shaderc_compile_options_clone ::
	ShadercCompileOptionsT -> IO ShadercCompileOptionsT

foreign import ccall "shaderc_compile_options_release"
	c_shaderc_compile_options_release ::
	ShadercCompileOptionsT -> IO ()

foreign import ccall "shaderc_compile_options_add_macro_definition"
	c_shaderc_compile_options_add_macro_definition ::
	ShadercCompileOptionsT -> Ptr CChar -> #{type size_t} ->
	Ptr CChar -> #{type size_t} -> IO ()

foreign import ccall "shaderc_compile_options_set_source_language"
	c_shaderc_compile_options_set_source_language ::
	ShadercCompileOptionsT -> #{type shaderc_source_language} -> IO ()

foreign import ccall "shaderc_compile_options_set_generate_debug_info"
	c_shaderc_compile_options_set_generate_debug_info ::
	ShadercCompileOptionsT -> IO ()

foreign import ccall "shaderc_compile_options_set_optimization_level"
	c_shaderc_compile_options_set_optimization_level ::
	ShadercCompileOptionsT -> #{type shaderc_optimization_level} -> IO ()

foreign import ccall "shaderc_compile_options_set_forced_version_profile"
	c_shaderc_compile_options_set_forced_version_profile ::
	ShadercCompileOptionsT -> #{type int} -> #{type shaderc_profile} -> IO()

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

foreign import ccall "shaderc_result_release" c_shaderc_result_release ::
	ShadercCompilationResultT -> IO ()

foreign import ccall "shaderc_result_get_length" c_shaderc_result_get_length ::
	ShadercCompilationResultT -> IO #{type size_t}

foreign import ccall "shaderc_result_get_bytes" c_shaderc_result_get_bytes ::
	ShadercCompilationResultT -> IO (Ptr CChar)

foreign import ccall "shaderc_result_get_error_message"
	c_shaderc_result_get_error_message ::
	ShadercCompilationResultT -> IO CString

shadercGlslVertexShader :: #{type shaderc_shader_kind}
shadercGlslVertexShader = #{const shaderc_glsl_vertex_shader}

shadercVertexShader :: #{type shaderc_shader_kind}
shadercVertexShader = #{const shaderc_vertex_shader}

shadercSourceLanguageHlsl :: #{type shaderc_source_language}
shadercSourceLanguageHlsl = #{const shaderc_source_language_hlsl}
