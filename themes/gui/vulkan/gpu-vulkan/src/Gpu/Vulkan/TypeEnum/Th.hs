{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Gpu.Vulkan.TypeEnum.Th where

import Language.Haskell.TH

mkInstance :: String -> Q Dec
mkInstance ss =
	instanceD (pure []) (conT (mkName "ShaderStageFlagBitsToValue") `appT` promotedT (mkName ss)) [
		valD (varP $ mkName "shaderStageFlagBitsToValue") (normalB . conE . mkName $ "E." ++ ss) []
		]
