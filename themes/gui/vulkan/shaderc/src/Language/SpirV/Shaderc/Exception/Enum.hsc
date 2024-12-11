{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Language.SpirV.Shaderc.Exception.Enum where

import Foreign.Storable
import Foreign.C.Enum
import Data.Word

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
