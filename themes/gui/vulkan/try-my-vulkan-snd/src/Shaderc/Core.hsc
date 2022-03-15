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

glslVertexShader :: #{type shaderc_shader_kind}
glslVertexShader = #{const shaderc_glsl_vertex_shader}

vertexShader :: #{type shaderc_shader_kind}
vertexShader = #{const shaderc_vertex_shader}

sourceLanguageHlsl :: #{type shaderc_source_language}
sourceLanguageHlsl = #{const shaderc_source_language_hlsl}

optimizationLevelZero :: #{type shaderc_optimization_level}
optimizationLevelZero = #{const shaderc_optimization_level_zero}

optimizationLevelSize :: #{type shaderc_optimization_level}
optimizationLevelSize = #{const shaderc_optimization_level_size}

optimizationLevelPerformance :: #{type shaderc_optimization_level}
optimizationLevelPerformance =
	#{const shaderc_optimization_level_performance}
