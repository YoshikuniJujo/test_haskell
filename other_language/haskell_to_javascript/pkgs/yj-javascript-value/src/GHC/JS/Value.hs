{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value (V(..), IsJSVal(..), Some, cast, consoleLog) where

import Data.Typeable qualified as Tp
import Data.Bool

import GHC.JS.Prim (JSVal, toJSString, toJSInt)

data Some = forall v . V v => Some v

class IsJSVal v where toJSVal :: v -> JSVal

class (IsJSVal v, Tp.Typeable v) => V v where
	toV :: v -> Some; fromV :: Some -> Maybe v
	toV = Some; fromV (Some v) = Tp.cast v

instance IsJSVal Some where toJSVal (Some v) = toJSVal v
instance V Some where toV = id; fromV = Just

cast :: (V v1, V v2) => v1 -> Maybe v2
cast = fromV . toV

instance IsJSVal String where toJSVal = (toJSString $!)
instance V String

instance IsJSVal Int where toJSVal = toJSInt
instance V Int

instance IsJSVal Bool where toJSVal = bool js_false js_true
instance V Bool

foreign import javascript "(() => { return false })" js_false :: JSVal
foreign import javascript "(() => { return true })" js_true :: JSVal

consoleLog :: Some -> IO ()
consoleLog o = js_consoleLog $ toJSVal o

foreign import javascript "((o) => { console.log(o); })"
	js_consoleLog :: JSVal -> IO ()
