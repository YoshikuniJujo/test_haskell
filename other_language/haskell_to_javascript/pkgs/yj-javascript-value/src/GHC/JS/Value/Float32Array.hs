{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Float32Array where

import Prelude hiding (IO)
import Prelude qualified as P

import Control.Monad.ST qualified as ST
import Control.Monad.ST.Unsafe

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object
import GHC.JS.Value.Array qualified as JS.Array

newtype F = F { unF :: JSVal }

instance Show F where
	show f = "(" ++ JS.Object.toString (JS.Object.toO f) ++ ")"

instance JS.Value.IsJSVal F where toJSVal (F v) = v
instance JS.Value.V F where toV = JS.Object.toValue; fromV = JS.Object.fromValue
instance JS.Object.IsO F

newtype IO = IO F deriving JS.Value.IsJSVal

newtype ST s = ST F deriving JS.Value.IsJSVal

new :: JS.Value.Some -> P.IO F
new (JS.Value.toJSVal -> o) = F <$> js_new o

class M fa m | m ->fa where
	new_ :: (JS.Array.M a m, JS.Value.IsJSVal a) => a -> m fa
	freeze :: fa -> m F; thaw :: F -> m fa

instance M IO P.IO where
	new_ a = IO . F <$> js_new (JS.Value.toJSVal a)
	freeze (JS.Value.toJSVal -> f) = F <$> js_new f
	thaw (JS.Value.toJSVal -> f) = IO . F <$> js_new f

instance M (ST s) (ST.ST s) where
	new_ a = unsafeIOToST $ ST . F <$> js_new (JS.Value.toJSVal a)
	freeze (JS.Value.toJSVal -> f) = F <$> unsafeIOToST (js_new f)
	thaw (JS.Value.toJSVal -> f) = ST . F <$> unsafeIOToST (js_new f)

fromList :: [Float] -> F
fromList fs = ST.runST $ freeze =<< fromListM fs

fromListM :: (Monad m, JS.Array.M a m, M b m, JS.Value.IsJSVal a) =>
	[Float] -> m b
fromListM fs = do
	a <- JS.Array.fromFloatListM fs
	new_ a

foreign import javascript "((o) => { const r = new Float32Array(o); return r })"
	js_new :: JSVal -> P.IO JSVal

byteLength :: F -> Int
byteLength = js_byteLength . unF

foreign import javascript "((f) => { return f.byteLength })"
	js_byteLength :: JSVal -> Int

length :: F -> Int
length = js_length . unF

foreign import javascript "((f) => { return f.length })"
	js_length :: JSVal -> Int
