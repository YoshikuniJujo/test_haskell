{-# LANGUAGE ViewPatterns #-}

module Tools where

import GHC.JS.Prim (JSVal, toJSString)
import GHC.JS.Value qualified as JS.Value

newtype TextEncoder = TextEncoder JSVal

instance JS.Value.IsJSVal TextEncoder where toJSVal (TextEncoder te) = te
instance JS.Value.V TextEncoder

newTextEncoder :: IO TextEncoder
newTextEncoder = TextEncoder <$> js_newTextEncoder

foreign import javascript "(() => { return new TextEncoder() })"
	js_newTextEncoder :: IO JSVal

encode :: TextEncoder -> String -> IO Uint8Array
encode (JS.Value.toJSVal -> te) (toJSString -> s) = Uint8Array <$> js_encode te s

foreign import javascript "((e, s) => { return e.encode(s) })"
	js_encode :: JSVal -> JSVal -> IO JSVal

newtype Uint8Array = Uint8Array JSVal

instance JS.Value.IsJSVal Uint8Array where toJSVal (Uint8Array uia) = uia
instance JS.Value.V Uint8Array
