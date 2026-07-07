module GHC.JS.Value.GpuTexture where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object

newtype G = G JSVal

instance JS.Value.IsJSVal G where toJSVal (G v) = v
instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue
