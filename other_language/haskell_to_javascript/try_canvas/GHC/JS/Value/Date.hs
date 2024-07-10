{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Date where

import GHC.JS.Prim
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object

newtype D = D JSVal

instance JS.Value.IsJSVal D where toJSVal (D v) = v
instance JS.Value.V D where toV = JS.Object.toValue; fromV = JS.Object.fromValue

instance JS.Object.IsO D

new :: IO D
new = D <$> js_new

foreign import javascript "(() => { return (new Date()); })" js_new :: IO JSVal
