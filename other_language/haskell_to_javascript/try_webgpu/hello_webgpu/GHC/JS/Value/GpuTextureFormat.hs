{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuTextureFormat where

import GHC.JS.Value qualified as JS.Value

newtype G = G String

pattern R8Unorm :: G
pattern R8Unorm = G "r8unorm"

instance Show G where
	show = \case
		R8Unorm -> "r8unorm"
		G g -> "(G " ++ show g ++ ")"

instance JS.Value.IsJSVal G where toJSVal (G g) = JS.Value.toJSVal g
instance JS.Value.V G
