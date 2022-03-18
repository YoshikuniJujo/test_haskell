{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.CompileOptions.Core where

import Foreign.Ptr
import Foreign.C.Types
import Data.Word

import Shaderc.Enum
import Shaderc.Include.Core

#include <shaderc/shaderc.h>

data Tag
type T = Ptr Tag

foreign import ccall "shaderc_compile_options_initialize" initialize :: IO T
foreign import ccall "shaderc_compile_options_clone" clone :: T -> IO T
foreign import ccall "shaderc_compile_options_release" release :: T -> IO ()

foreign import ccall "shaderc_compile_options_add_macro_definition"
	addMacroDefinition ::
	T -> Ptr CChar -> #{type size_t} -> Ptr CChar -> #{type size_t} -> IO ()

foreign import ccall "shaderc_compile_options_set_source_language"
	setSourceLanguage :: T -> SourceLanguage -> IO ()

foreign import ccall "shaderc_compile_options_set_generate_debug_info"
	setGenerateDebugInfo :: T -> IO ()

foreign import ccall "shaderc_compile_options_set_optimization_level"
	setOptimizationLevel :: T -> OptimizationLevel -> IO ()

foreign import ccall "shaderc_compile_options_set_forced_version_profile"
	setForcedVersionProfile :: T -> Version -> Profile -> IO()

setIncludeCallbacks :: T -> ResolveFn -> ResultReleaseFn -> PtrVoid -> IO ()
setIncludeCallbacks opts rfn rrfn ud = do
	prfn <- wrap_resolveFn rfn
	prrfn <- wrap_resultReleaseFn rrfn
	c_shaderc_compile_options_set_include_callbacks opts prfn prrfn ud

foreign import ccall "shaderc_compile_options_set_include_callbacks"
	c_shaderc_compile_options_set_include_callbacks ::
	T -> FunPtr ResolveFn -> FunPtr ResultReleaseFn -> PtrVoid -> IO ()
