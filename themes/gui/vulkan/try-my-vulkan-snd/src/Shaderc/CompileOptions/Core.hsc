{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.CompileOptions.Core where

import Foreign.Ptr
import Foreign.C.Types
import Data.Word
import Data.Int

import Shaderc.Core
import Shaderc.Include.Core

#include <shaderc/shaderc.h>

foreign import ccall "shaderc_compile_options_initialize"
	initialize :: IO CompileOptionsT

foreign import ccall "shaderc_compile_options_clone"
	clone :: CompileOptionsT -> IO CompileOptionsT

foreign import ccall "shaderc_compile_options_release"
	release :: CompileOptionsT -> IO ()

foreign import ccall "shaderc_compile_options_add_macro_definition"
	addMacroDefinition ::
	CompileOptionsT -> Ptr CChar -> #{type size_t} ->
	Ptr CChar -> #{type size_t} -> IO ()

foreign import ccall "shaderc_compile_options_set_source_language"
	setSourceLanguage ::
	CompileOptionsT -> #{type shaderc_source_language} -> IO ()

foreign import ccall "shaderc_compile_options_set_generate_debug_info"
	setGenerateDebugInfo :: CompileOptionsT -> IO ()

foreign import ccall "shaderc_compile_options_set_optimization_level"
	setOptimizationLevel ::
	CompileOptionsT -> #{type shaderc_optimization_level} -> IO ()

foreign import ccall "shaderc_compile_options_set_forced_version_profile"
	setForcedVersionProfile ::
	CompileOptionsT -> #{type int} -> #{type shaderc_profile} -> IO()

shadercGlslVertexShader :: #{type shaderc_shader_kind}
shadercGlslVertexShader = #{const shaderc_glsl_vertex_shader}

shadercVertexShader :: #{type shaderc_shader_kind}
shadercVertexShader = #{const shaderc_vertex_shader}

shadercSourceLanguageHlsl :: #{type shaderc_source_language}
shadercSourceLanguageHlsl = #{const shaderc_source_language_hlsl}

shadercOptimizationLevelZero :: #{type shaderc_optimization_level}
shadercOptimizationLevelZero = #{const shaderc_optimization_level_zero}

shadercOptimizationLevelSize :: #{type shaderc_optimization_level}
shadercOptimizationLevelSize = #{const shaderc_optimization_level_size}

shadercOptimizationLevelPerformance :: #{type shaderc_optimization_level}
shadercOptimizationLevelPerformance =
	#{const shaderc_optimization_level_performance}

setIncludeCallbacks ::
	CompileOptionsT -> ResolveFn -> ResultReleaseFn -> PtrVoid ->
	IO ()
setIncludeCallbacks opts rfn rrfn ud = do
	prfn <- wrap_resolveFn rfn
	prrfn <- wrap_resultReleaseFn rrfn
	c_shaderc_compile_options_set_include_callbacks opts prfn prrfn ud

foreign import ccall "shaderc_compile_options_set_include_callbacks"
	c_shaderc_compile_options_set_include_callbacks ::
	CompileOptionsT -> FunPtr ResolveFn -> FunPtr ResultReleaseFn ->
	PtrVoid -> IO ()
