{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import GHC.JS.Prim (JSVal, toJSString)
import GHC.JS.Value qualified as JS.Value

import Control.Arrow

main :: IO ()
main = demoECDSA

demoECDSA :: IO ()
demoECDSA = do
	(sk, pk) <- keyPair
	JS.Value.consoleLog sk
	JS.Value.consoleLog pk
	e <- newTextEncoder
	dt <- encode e "われ泣きぬれて蟹みそを食べる"
	JS.Value.consoleLog =<< sign sk dt

keyPair :: IO (PrivateKey, PublicKey)
keyPair = (PrivateKey . js_privateKey &&& PublicKey . js_publicKey) <$> js_keyPair

foreign import javascript interruptible "((cont) => { globalThis.crypto.subtle.generateKey( { name: 'ECDSA', namedCurve: 'P-256' }, true, ['sign', 'verify'] ).then((p) => cont(p)) })"
	js_keyPair :: IO JSVal

newtype PrivateKey = PrivateKey JSVal
instance JS.Value.IsJSVal PrivateKey where toJSVal (PrivateKey pk) = pk
instance JS.Value.V PrivateKey

newtype PublicKey = PublicKey JSVal
instance JS.Value.IsJSVal PublicKey where toJSVal (PublicKey pk) = pk
instance JS.Value.V PublicKey

foreign import javascript "((p) => { return p.privateKey })" js_privateKey :: JSVal -> JSVal
foreign import javascript "((p) => { return p.publicKey })" js_publicKey :: JSVal -> JSVal

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

sign :: PrivateKey -> Uint8Array -> IO ArrayBufferUint8
sign (JS.Value.toJSVal -> sk) (JS.Value.toJSVal -> dt) = ArrayBufferUint8 <$> js_sign sk dt

foreign import javascript interruptible "((sk, dt, cont) => { globalThis.crypto.subtle.sign({ name : 'ECDSA', hash: { name: 'SHA-256' } }, sk, dt).then((s) => cont(s)) })"
	js_sign :: JSVal -> JSVal -> IO JSVal

newtype ArrayBufferUint8 = ArrayBufferUint8 JSVal
instance JS.Value.IsJSVal ArrayBufferUint8 where toJSVal (ArrayBufferUint8 a) = a
instance JS.Value.V ArrayBufferUint8
