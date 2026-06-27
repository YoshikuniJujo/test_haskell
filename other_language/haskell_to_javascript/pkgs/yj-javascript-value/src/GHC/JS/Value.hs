{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value (V(..), IsJSVal(..), Some, cast, consoleLog) where

import Data.Typeable qualified as Tp
import Data.Bool
import Data.Word
import System.IO.Unsafe

import GHC.JS.Prim (JSVal, toJSString, toJSInt)

import {-# SOURCE #-} GHC.JS.Value.Array qualified as JS.Array

data Some = forall v . V v => Some v

class V v => IsJSVal v where
	toJSVal :: v -> JSVal
	toJSValList :: [v] -> JSVal

	toJSValList = unsafePerformIO . (JS.Array.unA <$>) . fromListIO

fromListIO :: V a => [a] -> IO JS.Array.A
fromListIO xs = do
	a <- JS.Array.new
	mapM_ (push a) (toV <$> xs)
	pure a

push :: JS.Array.A -> Some -> IO ()
push (JS.Array.A a) (toJSVal -> x) = JS.Array.js_push a x

class (IsJSVal v, Tp.Typeable v) => V v where
	toV :: v -> Some; fromV :: Some -> Maybe v
	toV = Some; fromV (Some v) = Tp.cast v

instance IsJSVal Some where toJSVal (Some v) = toJSVal v
instance V Some where toV = id; fromV = Just

cast :: (V v1, V v2) => v1 -> Maybe v2
cast = fromV . toV

instance IsJSVal a => IsJSVal [a] where toJSVal = toJSVal
instance V a => V [a]

instance IsJSVal Char where toJSValList = (toJSString $!)
instance V Char

{-
instance IsJSVal String where toJSVal = (toJSString $!)
instance V String
-}

instance IsJSVal Int where toJSVal = toJSInt
instance V Int

instance IsJSVal Word32 where toJSVal = toJSInt . fromIntegral
instance V Word32

instance IsJSVal Bool where toJSVal = bool js_false js_true
instance V Bool

foreign import javascript "(() => { return false })" js_false :: JSVal
foreign import javascript "(() => { return true })" js_true :: JSVal

consoleLog :: Some -> IO ()
consoleLog o = js_consoleLog $ toJSVal o

foreign import javascript "((o) => { console.log(o); })"
	js_consoleLog :: JSVal -> IO ()
