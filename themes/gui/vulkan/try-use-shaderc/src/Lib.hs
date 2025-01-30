{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Lib where

import Data.Default
import Data.ByteString qualified as BS
import Text.Nowdoc
import Language.SpirV
import Language.SpirV.Shaderc
import Language.SpirV.Shaderc.CompileOptions
import Language.SpirV.ShaderKind

getBinary :: IO (S GlslVertexShader)
getBinary = compile sample "" "main" (def :: C ())

sample :: BS.ByteString
sample = [nowdoc|

#version 460

layout(location = 0) in vec3 in0;

layout(location = 0) out vec4 outColor;

layout(std140, binding = 0) uniform UBO0 {
  float _0;
} un0;

void main() {
  vec4 t0 = vec4(in0.x, in0.y, in0.z, un0.0); // error - should be un0._0
  outColor = t0;
}

|]
