{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.Options.Core where

import Foreign.Ptr
import Foreign.C.Types
import Data.Word
import Data.Int

#include <shaderc/shaderc.h>

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
