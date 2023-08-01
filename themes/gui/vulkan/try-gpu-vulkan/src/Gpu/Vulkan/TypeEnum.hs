{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.TypeEnum where

import Language.Haskell.TH
import Data.Bits

import Gpu.Vulkan.TypeEnum.Th

import qualified Gpu.Vulkan.Enum as E

import Gpu.Vulkan.TypeEnum.TypeValues

do	is <- lines <$> runIO (readFile "th/vkShaderStageFlagBits.txt")
	(: []) <$> dataD (pure []) (mkName "ShaderStageFlagBits") [] Nothing
		((`normalC` []) . mkName <$> is)
		[]

class ShaderStageFlagBitsListToValue (ts :: [ShaderStageFlagBits]) where
	shaderStageFlagBitsListToValue :: E.ShaderStageFlagBits

instance ShaderStageFlagBitsListToValue '[] where
	shaderStageFlagBitsListToValue = zeroBits

instance (ShaderStageFlagBitsToValue t, ShaderStageFlagBitsListToValue ts) =>
	ShaderStageFlagBitsListToValue (t ': ts) where
	shaderStageFlagBitsListToValue =
		shaderStageFlagBitsToValue @t .|.
		shaderStageFlagBitsListToValue @ts

class ShaderStageFlagBitsToValue (t :: ShaderStageFlagBits) where
	shaderStageFlagBitsToValue :: E.ShaderStageFlagBits

do	is <- lines <$> runIO (readFile "th/vkShaderStageFlagBits.txt")
	sequence $ mkInstance <$> is

typeValues "E" "Format" =<< lines <$> runIO (readFile "th/vkFormat.txt")
