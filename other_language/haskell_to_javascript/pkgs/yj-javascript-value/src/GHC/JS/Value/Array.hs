{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Array where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object

newtype A = A { unA :: JSVal }

instance Show A where
	show a = "(" ++ JS.Object.toString (JS.Object.toO a) ++ ")"

instance JS.Value.IsJSVal A where toJSVal (A v) = v
instance JS.Value.V A where toV = JS.Object.toValue; fromV = JS.Object.fromValue

instance JS.Object.IsO A

new :: IO A
new = A <$> js_new

foreign import javascript "(() => { return Array() })" js_new :: IO JSVal

fromList :: JS.Value.V a => [a] -> IO A
fromList xs = do
	a <- new
	mapM_ (push a) (JS.Value.toV <$> xs)
	pure a

push :: A -> JS.Value.Some -> IO ()
push (A a) (JS.Value.toJSVal -> x) = js_push a x

foreign import javascript "((a, x) => { a.push(x); })" js_push :: JSVal -> JSVal -> IO ()

fromFloatList :: [Float] -> IO A
fromFloatList fs = new >>= \a -> a <$ mapM_ (pushFloat a) fs

pushFloat :: A -> Float -> IO ()
pushFloat (A a) f = js_push_float a f

foreign import javascript "((a, f) => { a.push(f); })" js_push_float :: JSVal -> Float -> IO ()
