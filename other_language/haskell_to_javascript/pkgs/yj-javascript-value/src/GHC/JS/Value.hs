{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value (V(..), IsJSVal(..), Some, cast, consoleLog, js_consoleLog) where

import Data.Typeable qualified as Tp
import Data.Bool
import Data.Word
import System.IO.Unsafe

import GHC.JS.Prim (JSVal, toJSString, toJSInt)

data Some = forall v . V v => Some v

class IsJSVal v where
	toJSVal :: v -> JSVal
	toJSValList :: [v] -> JSVal
	toJSValList = unsafePerformIO . fromListRaw . (toJSVal <$>)

class (IsJSVal v, Tp.Typeable v) => V v where
	toV :: v -> Some; fromV :: Some -> Maybe v
	toV = Some; fromV (Some v) = Tp.cast v

fromListRaw :: [JSVal] -> IO JSVal
fromListRaw xs = do
	a <- js_new
	mapM_ (js_push a) xs
	pure a

instance IsJSVal v => IsJSVal [v] where toJSVal = toJSValList
instance (IsJSVal v, Tp.Typeable v) => V [v]

foreign import javascript "(() => { return Array() })" js_new :: IO JSVal

foreign import javascript "((a, x) => { a.push(x); })" js_push :: JSVal -> JSVal -> IO ()

instance IsJSVal Some where toJSVal (Some v) = toJSVal v
instance V Some where toV = id; fromV = Just

cast :: (V v1, V v2) => v1 -> Maybe v2
cast = fromV . toV

instance IsJSVal Char where toJSValList = (toJSString $!)

instance IsJSVal Int where toJSVal = toJSInt
instance V Int

instance IsJSVal Word32 where toJSVal = toJSInt . fromIntegral
instance V Word32

instance IsJSVal Double where toJSVal = toJSDouble
instance V Double

foreign import javascript "((o) => { return o })" toJSDouble :: Double -> JSVal

instance IsJSVal Bool where toJSVal = bool js_false js_true
instance V Bool

foreign import javascript "(() => { return false })" js_false :: JSVal
foreign import javascript "(() => { return true })" js_true :: JSVal

consoleLog :: Some -> IO ()
consoleLog o = js_consoleLog $ toJSVal o

foreign import javascript "((o) => { console.log(o); })"
	js_consoleLog :: JSVal -> IO ()
