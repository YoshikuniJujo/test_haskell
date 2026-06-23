{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuVertexFormat where

import GHC.JS.Value qualified as JS.Value

newtype G = G String

pattern GUint8 :: G
pattern GUint8 = G "uint8"

pattern GFloat32x4 :: G
pattern GFloat32x4 = G "float32x4"

instance Show G where
	show = \case
		GUint8 -> "GUint8"
		GFloat32x4 -> "GFloat32x4"
		G g -> "(G " ++ show g ++ ")"

instance JS.Value.IsJSVal G where toJSVal (G g) = JS.Value.toJSVal g
instance JS.Value.V G
