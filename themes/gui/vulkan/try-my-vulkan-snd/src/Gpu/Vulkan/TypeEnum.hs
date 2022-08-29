{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes, TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.TypeEnum where

import Language.Haskell.TH

import Gpu.Vulkan.TypeEnum.Th

import qualified Gpu.Vulkan.Enum as E

import TypeValues

do	is <- lines <$> runIO (readFile "th/vkShaderStageFlagBits.txt")
	(: []) <$> dataD (pure []) (mkName "ShaderStageFlagBits") [] Nothing
		((`normalC` []) . mkName <$> is)
		[]

class ShaderStageFlagBitsToValue (t :: ShaderStageFlagBits) where
	shaderStageFlagBitsToValue :: E.ShaderStageFlagBits

do	is <- lines <$> runIO (readFile "th/vkShaderStageFlagBits.txt")
	sequence $ mkInstance <$> is

typeValues "E" "Format" =<< lines <$> runIO (readFile "th/vkFormat.txt")
