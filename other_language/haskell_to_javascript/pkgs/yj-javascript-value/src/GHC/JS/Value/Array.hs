{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Array where

import Prelude hiding (IO)
import Prelude qualified as P
import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object

newtype A = A { unA :: JSVal }

instance Show A where
	show a = "(" ++ JS.Object.toString (JS.Object.toO a) ++ ")"

instance JS.Value.IsJSVal A where toJSVal (A v) = v
instance JS.Value.V A where toV = JS.Object.toValue; fromV = JS.Object.fromValue

instance JS.Object.IsO A

newtype IO = IO A

newtype ST = ST A

class M a m where
	new_ :: m a
	push_ :: JS.Value.V v => a -> v -> m ()
	freeze :: a -> m A; thaw :: A -> m a

new :: P.IO A
new = A <$> js_new

foreign import javascript "(() => { return Array() })" js_new :: P.IO JSVal

fromListIO :: JS.Value.V a => [a] -> P.IO A
fromListIO xs = do
	a <- new
	mapM_ (push a) (JS.Value.toV <$> xs)
	pure a

push :: JS.Value.V v => A -> v -> P.IO ()
push (A a) (JS.Value.toJSVal -> x) = js_push a x

foreign import javascript "((a, x) => { a.push(x); })" js_push :: JSVal -> JSVal -> P.IO ()

fromFloatList :: [Float] -> P.IO A
fromFloatList fs = new >>= \a -> a <$ mapM_ (push a) fs

foreign import javascript "((a, f) => { a.push(f); })" js_push_float :: JSVal -> Float -> P.IO ()
