{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.JS.Value.Object (
	O, toValue, fromValue, IsO, toO, otherO,
	isInstanceOf, Class(..), toString, getInt,

	M(..), IO, ST
	) where

import Prelude qualified as P
import Prelude hiding (IO)
import Control.Monad.ST qualified as ST
import Control.Monad.ST.Unsafe qualified as ST
import GHC.JS.Prim (JSVal, fromJSString, toJSString, fromJSInt)
import GHC.JS.Value qualified as JS.Value
import Data.Typeable (cast)
import Data.Maybe (fromJust)

data O = forall o . JS.Value.V o => O o

class M o m | m -> o where
	new :: m o
	set :: JS.Value.V v => o -> String -> v -> m ()
	freeze :: o -> m O
	thaw :: O -> m o

newtype IO = IO O deriving JS.Value.IsJSVal

instance M IO P.IO where
	new = toIO . OtherO <$> js_new
	set (JS.Value.toJSVal -> o) (toJSString -> k) (JS.Value.toJSVal -> v) =
		js_set o k v
	freeze (JS.Value.toJSVal -> o) = toO . OtherO <$> js_shallowCopy o
	thaw (JS.Value.toJSVal -> o) = toIO . OtherO <$> js_shallowCopy o

newtype ST s = ST O deriving JS.Value.IsJSVal

instance M (ST s) (ST.ST s) where
	new = ST.unsafeIOToST $ toST . OtherO <$> js_new
	set (JS.Value.toJSVal -> o) (toJSString -> k) (JS.Value.toJSVal -> v) =
		ST.unsafeIOToST $ js_set o k v
	freeze (JS.Value.toJSVal -> o) =
		ST.unsafeIOToST $ toO . OtherO <$> js_shallowCopy o
	thaw (JS.Value.toJSVal -> o) =
		ST.unsafeIOToST $ toST . OtherO <$> js_shallowCopy o

foreign import javascript "((o) => { return { ...o }; })"
	js_shallowCopy :: JSVal -> P.IO JSVal

instance Show O where show = toString

instance JS.Value.IsJSVal O where toJSVal (O o) = JS.Value.toJSVal o
instance JS.Value.V O

toValue :: JS.Value.V o => o -> JS.Value.Some
toValue = JS.Value.toV . O

fromValue :: JS.Value.V o => JS.Value.Some -> Maybe o
fromValue v = JS.Value.fromV v >>= \(O o) -> cast o

toO :: IsO o => o -> O
toO = fromJust . JS.Value.cast

toIO :: IsO o => o -> IO
toIO = IO . toO

toST :: IsO o => o -> ST s
toST = ST . toO

class JS.Value.V o => IsO o

isInstanceOf :: O -> Class -> Bool
o `isInstanceOf` Class c = JS.Value.toJSVal o `js_instanceof` c

foreign import javascript "((o, c) => { return (o instanceof c); })"
	js_instanceof :: JSVal -> JSVal -> Bool

newtype Class = Class JSVal

toString :: O -> String
toString obj = fromJSString . js_toString $ JS.Value.toJSVal obj

foreign import javascript "((o) => { return o.toString(); })"
	js_toString :: JSVal -> JSVal

foreign import javascript "(() => { return {} })" js_new :: P.IO JSVal

foreign import javascript "((o, k, v) => { o[k] = v; })"
	js_set :: JSVal -> JSVal -> JSVal -> P.IO ()

foreign import javascript "((o, k) => { return o[k]; })"
	js_get :: JSVal -> JSVal -> P.IO JSVal

getInt :: O -> String -> P.IO (Maybe Int)
getInt (JS.Value.toJSVal -> o) (toJSString -> k) = fromJSInt' <$> js_get o k

fromJSInt' :: JSVal -> Maybe Int
fromJSInt' v
	| js_isInteger v = Just $ fromJSInt v
	| otherwise = Nothing

foreign import javascript "((v) => { return Number.isInteger(v) })"
	js_isInteger :: JSVal -> Bool

newtype OtherO = OtherO JSVal

instance JS.Value.IsJSVal OtherO where toJSVal (OtherO v) = v
instance JS.Value.V OtherO where toV = toValue; fromV = fromValue

instance IsO OtherO

otherO :: JSVal -> O
otherO = toO . OtherO
