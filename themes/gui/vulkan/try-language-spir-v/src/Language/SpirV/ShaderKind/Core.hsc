-- This file is automatically generated by the tools/makeShadercShaderKind.hs
--	% stack runghc --cwd tools/ makeShadercShaderKind

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Language.SpirV.ShaderKind.Core where

import Foreign.Storable
import Foreign.C.Enum
import Data.Word

#include <shaderc/shaderc.h>

enum "ShaderKind" ''#{type shaderc_shader_kind} [''Show, ''Eq, ''Storable] [
	("VertexShader", #{const shaderc_vertex_shader}),
	("FragmentShader", #{const shaderc_fragment_shader}),
	("ComputeShader", #{const shaderc_compute_shader}),
	("GeometryShader", #{const shaderc_geometry_shader}),
	("TessControlShader", #{const shaderc_tess_control_shader}),
	("TessEvaluationShader", #{const shaderc_tess_evaluation_shader}),
	("GlslVertexShader", #{const shaderc_glsl_vertex_shader}),
	("GlslFragmentShader", #{const shaderc_glsl_fragment_shader}),
	("GlslComputeShader", #{const shaderc_glsl_compute_shader}),
	("GlslGeometryShader", #{const shaderc_glsl_geometry_shader}),
	("GlslTessControlShader", #{const shaderc_glsl_tess_control_shader}),
	("GlslTessEvaluationShader",
		#{const shaderc_glsl_tess_evaluation_shader}),
	("GlslInferFromSource", #{const shaderc_glsl_infer_from_source}),
	("GlslDefaultVertexShader",
		#{const shaderc_glsl_default_vertex_shader}),
	("GlslDefaultFragmentShader",
		#{const shaderc_glsl_default_fragment_shader}),
	("GlslDefaultComputeShader",
		#{const shaderc_glsl_default_compute_shader}),
	("GlslDefaultGeometryShader",
		#{const shaderc_glsl_default_geometry_shader}),
	("GlslDefaultTessControlShader",
		#{const shaderc_glsl_default_tess_control_shader}),
	("GlslDefaultTessEvaluationShader",
		#{const shaderc_glsl_default_tess_evaluation_shader}),
	("SpirvAssembly", #{const shaderc_spirv_assembly}),
	("RaygenShader", #{const shaderc_raygen_shader}),
	("AnyhitShader", #{const shaderc_anyhit_shader}),
	("ClosesthitShader", #{const shaderc_closesthit_shader}),
	("MissShader", #{const shaderc_miss_shader}),
	("IntersectionShader", #{const shaderc_intersection_shader}),
	("CallableShader", #{const shaderc_callable_shader}),
	("GlslRaygenShader", #{const shaderc_glsl_raygen_shader}),
	("GlslAnyhitShader", #{const shaderc_glsl_anyhit_shader}),
	("GlslClosesthitShader", #{const shaderc_glsl_closesthit_shader}),
	("GlslMissShader", #{const shaderc_glsl_miss_shader}),
	("GlslIntersectionShader", #{const shaderc_glsl_intersection_shader}),
	("GlslCallableShader", #{const shaderc_glsl_callable_shader}),
	("GlslDefaultRaygenShader",
		#{const shaderc_glsl_default_raygen_shader}),
	("GlslDefaultAnyhitShader",
		#{const shaderc_glsl_default_anyhit_shader}),
	("GlslDefaultClosesthitShader",
		#{const shaderc_glsl_default_closesthit_shader}),
	("GlslDefaultMissShader", #{const shaderc_glsl_default_miss_shader}),
	("GlslDefaultIntersectionShader",
		#{const shaderc_glsl_default_intersection_shader}),
	("GlslDefaultCallableShader",
		#{const shaderc_glsl_default_callable_shader}),
	("TaskShader", #{const shaderc_task_shader}),
	("MeshShader", #{const shaderc_mesh_shader}),
	("GlslTaskShader", #{const shaderc_glsl_task_shader}),
	("GlslMeshShader", #{const shaderc_glsl_mesh_shader}),
	("GlslDefaultTaskShader", #{const shaderc_glsl_default_task_shader}),
	("GlslDefaultMeshShader", #{const shaderc_glsl_default_mesh_shader}) ]
