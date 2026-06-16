{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.GpuDevice where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.EventTarget qualified as JS.EventTarget

newtype G = G JSVal

instance Show G where
	show g = "(" ++ JS.Object.toString (JS.Object.toO g) ++ ")"

instance JS.Value.IsJSVal G where toJSVal (G v) = v

instance JS.Value.V G where
	toV = JS.EventTarget.toValue; fromV = JS.EventTarget.fromValue

instance JS.Object.IsO G
instance JS.EventTarget.IsE G
