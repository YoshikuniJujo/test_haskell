module GHC.JS.Value.GpuShaderModule where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object

newtype G = G { unG :: JSVal }

instance Show G where
	show g = "(" ++ JS.Object.toString (JS.Object.toO g) ++ ")"

instance JS.Value.IsJSVal G where toJSVal (G v) = v
instance JS.Value.V G where toV = JS.Object.toValue; fromV = JS.Object.fromValue

instance JS.Object.IsO G
