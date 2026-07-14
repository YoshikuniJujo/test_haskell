{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import GHC.JS.Prim (JSVal, toJSString)
import GHC.JS.Value qualified as JS.Value

main :: IO ()
main = demoECDSA

demoECDSA :: IO ()
demoECDSA = do
	JS.Value.js_consoleLog =<< js_keyPair
	e <- newTextEncoder
	JS.Value.js_consoleLog =<< encode e "われ泣きぬれて蟹みそを食べる"

foreign import javascript interruptible "((cont) => { globalThis.crypto.subtle.generateKey( { name: 'ECDSA', namedCurve: 'P-256' }, true, ['sign', 'verify'] ).then((p) => cont(p)) })"
	js_keyPair :: IO JSVal

newtype TextEncoder = TextEncoder JSVal

instance JS.Value.IsJSVal TextEncoder where toJSVal (TextEncoder te) = te
instance JS.Value.V TextEncoder

newTextEncoder :: IO TextEncoder
newTextEncoder = TextEncoder <$> js_newTextEncoder

foreign import javascript "(() => { return new TextEncoder() })"
	js_newTextEncoder :: IO JSVal

encode :: TextEncoder -> String -> IO JSVal
encode (JS.Value.toJSVal -> te) (toJSString -> s) = js_encode te s

foreign import javascript "((e, s) => { return e.encode(s) })"
	js_encode :: JSVal -> JSVal -> IO JSVal
