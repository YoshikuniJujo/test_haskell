{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Array where

import Prelude hiding (IO)
import Prelude qualified as P

import Control.Monad.ST qualified as ST
import Control.Monad.ST.Unsafe

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object

newtype A = A { unA :: JSVal }

instance Show A where
	show a = "(" ++ JS.Object.toString (JS.Object.toO a) ++ ")"

instance JS.Value.IsJSVal A where toJSVal (A v) = v
instance JS.Value.V A where toV = JS.Object.toValue; fromV = JS.Object.fromValue
instance JS.Object.IsO A

newtype IO = IO A deriving JS.Value.IsJSVal

instance JS.Value.V IO where toV = JS.Object.toValue; fromV = JS.Object.fromValue

newtype ST s = ST A deriving JS.Value.IsJSVal

-- instance JS.Value.V (ST s) where toV = JS.Object.toValue; fromV = JS.Object.fromValue

class M a m | m -> a where
	new_ :: m a
	push_ :: JS.Value.V v => a -> v -> m ()
	freeze :: a -> m A; thaw :: A -> m a

instance M IO P.IO where
	new_ = IO . A <$> js_newIO
	push_ (IO (A a)) (JS.Value.toJSVal -> x) = js_pushIO a x
	freeze (JS.Value.toJSVal -> a) = A <$> js_shallowCopy a
	thaw (JS.Value.toJSVal -> a) = IO . A <$> js_shallowCopy a

foreign import javascript "((o) => { return [ ...o ]; })"
	js_shallowCopy :: JSVal -> P.IO JSVal

instance M (ST s) (ST.ST s) where
	new_ = ST . A <$> unsafeIOToST js_newIO
	push_ (ST (A a)) (JS.Value.toJSVal -> x) = unsafeIOToST $ js_pushIO a x
	freeze (JS.Value.toJSVal -> a) = A <$> unsafeIOToST (js_shallowCopy a)
	thaw (JS.Value.toJSVal -> a) = ST . A <$> unsafeIOToST (js_shallowCopy a)

foreign import javascript "(() => { return Array() })" js_newIO :: P.IO JSVal

fromListM :: (Monad m, M a m, JS.Value.V x) => [x] -> m a
fromListM xs = do
	a <- new_
	mapM_ (push_ a) (JS.Value.toV <$> xs)
	pure a

fromListIO :: JS.Value.V a => [a] -> P.IO IO
fromListIO = fromListM

foreign import javascript "((a, x) => { a.push(x); })"
	js_pushIO :: JSVal -> JSVal -> P.IO ()

fromFloatList :: [Float] -> A
fromFloatList fs = ST.runST $ freeze =<< fromFloatListM fs

fromFloatListM :: (Monad m, M a m) => [Float] -> m a
fromFloatListM = fromListM

foreign import javascript "((a, f) => { a.push(f); })" js_push_float :: JSVal -> Float -> P.IO ()
