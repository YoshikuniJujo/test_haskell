{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Shaderc.Enum where

import Foreign.Storable
import Foreign.C.Enum
import Data.Word
import Data.Int

#include <shaderc/shaderc.h>

enum "CompilationStatus" ''#{type shaderc_compilation_status}
		[''Show, ''Storable] [
	("CompilationStatusSuccess",
		#{const shaderc_compilation_status_success}),
	("CompilationStatusInvalidStage",
		#{const shaderc_compilation_status_invalid_stage}),
	("CompilationStatusError",
		#{const shaderc_compilation_status_compilation_error}),
	("CompilationStatusInternalError",
		#{const shaderc_compilation_status_internal_error}),
	("CompilationStatusNullResultObject",
		#{const shaderc_compilation_status_null_result_object}),
	("CompilationStatusInvalidAssembly",
		#{const shaderc_compilation_status_invalid_assembly}),
	("CompilationStatusValidationError",
		#{const shaderc_compilation_status_validation_error}),
	("CompilationStatusTransformationError",
		#{const shaderc_compilation_status_transformation_error}),
	("CompilationStatusConfigurationError",
		#{const shaderc_compilation_status_configuration_error}) ]

enum "SourceLanguage" ''#{type shaderc_source_language}
		[''Show, ''Storable] [
	("SourceLanguageGlsl", #{const shaderc_source_language_glsl}),
	("SourceLanguageHlsl", #{const shaderc_source_language_hlsl}) ]

enum "OptimizationLevel" ''#{type shaderc_optimization_level}
		[''Show, ''Eq, ''Storable] [
	("OptimizationLevelZero", #{const shaderc_optimization_level_zero}),
	("OptimizationLevelSize", #{const shaderc_optimization_level_size}),
	("OptimizationLevelPerformance",
		#{const shaderc_optimization_level_performance}) ]

enum "Profile" ''#{type shaderc_profile} [''Show, ''Eq, ''Storable] [
	("ProfileNone", #{const shaderc_profile_none}),
	("ProfileCore", #{const shaderc_profile_core}),
	("ProfileCompatibility", #{const shaderc_profile_compatibility}),
	("ProfileEs", #{const shaderc_profile_es}) ]

version :: Int -> Version
version = Version . fromIntegral

newtype Version = Version #{type int} deriving Show
