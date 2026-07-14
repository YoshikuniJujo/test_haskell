{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import GHC.JS.Prim (JSVal, toJSString)
import GHC.JS.Value qualified as JS.Value

import Control.Arrow

import Tools

main :: IO ()
main = demoECDSA

demoECDSA :: IO ()
demoECDSA = do
	(sk, pk) <- keyPair
	JS.Value.consoleLog sk
	JS.Value.consoleLog pk
	e <- newTextEncoder
	dt <- encode e "われ泣きぬれて蟹みそを食べる"
	s <- sign sk dt
	JS.Value.js_consoleLog =<< verify pk s dt

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

sign :: PrivateKey -> Uint8Array -> IO ArrayBufferUint8
sign (JS.Value.toJSVal -> sk) (JS.Value.toJSVal -> dt) = ArrayBufferUint8 <$> js_sign sk dt

foreign import javascript interruptible "((sk, dt, cont) => { globalThis.crypto.subtle.sign({ name : 'ECDSA', hash: { name: 'SHA-256' } }, sk, dt).then((s) => cont(s)) })"
	js_sign :: JSVal -> JSVal -> IO JSVal

newtype ArrayBufferUint8 = ArrayBufferUint8 JSVal
instance JS.Value.IsJSVal ArrayBufferUint8 where toJSVal (ArrayBufferUint8 a) = a
instance JS.Value.V ArrayBufferUint8

verify :: PublicKey -> ArrayBufferUint8 -> Uint8Array -> IO JSVal
verify (JS.Value.toJSVal -> pk) (JS.Value.toJSVal -> s) (JS.Value.toJSVal -> dt) = js_verify pk s dt

foreign import javascript interruptible "((pk, s, dt, cont) => { globalThis.crypto.subtle.verify({ name: 'ECDSA', hash: { name: 'SHA-256' } }, pk, s, dt).then((b) => cont(b)) })"
	js_verify :: JSVal -> JSVal -> JSVal -> IO JSVal
