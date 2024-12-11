{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.CompileOptions.Core where

import Foreign.Ptr
import Foreign.C.Types
import Data.Word

import Shaderc.Enum
import Shaderc.Include.Core

#include <shaderc/shaderc.h>

data CTag
type C = Ptr CTag

foreign import ccall "shaderc_compile_options_initialize" initialize :: IO C
foreign import ccall "shaderc_compile_options_clone" clone :: C -> IO C
foreign import ccall "shaderc_compile_options_release" release :: C -> IO ()

foreign import ccall "shaderc_compile_options_add_macro_definition"
	addMacroDefinition ::
	C -> Ptr CChar -> #{type size_t} -> Ptr CChar -> #{type size_t} -> IO ()

foreign import ccall "shaderc_compile_options_set_source_language"
	setSourceLanguage :: C -> SourceLanguage -> IO ()

foreign import ccall "shaderc_compile_options_set_generate_debug_info"
	setGenerateDebugInfo :: C -> IO ()

foreign import ccall "shaderc_compile_options_set_optimization_level"
	setOptimizationLevel :: C -> OptimizationLevel -> IO ()

foreign import ccall "shaderc_compile_options_set_forced_version_profile"
	setForcedVersionProfile :: C -> Version -> Profile -> IO()

setIncludeCallbacks :: C -> ResolveFn -> ResultReleaseFn -> PtrVoid -> IO ()
setIncludeCallbacks opts rfn rrfn ud = do
	prfn <- wrap_resolveFn rfn
	prrfn <- wrap_resultReleaseFn rrfn
	c_shaderc_compile_options_set_include_callbacks opts prfn prrfn ud

foreign import ccall "shaderc_compile_options_set_include_callbacks"
	c_shaderc_compile_options_set_include_callbacks ::
	C -> FunPtr ResolveFn -> FunPtr ResultReleaseFn -> PtrVoid -> IO ()
