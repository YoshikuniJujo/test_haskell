{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.JS.Prim (JSVal, toJSString)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Value.Object qualified as JS.Object

import Tools

main :: IO ()
main = do
	salt <- js_getRandomValues 16
	JS.Value.consoleLog salt
	aesKey <- generateKeyFromPassword "foo" salt
	JS.Value.consoleLog aesKey
--	JS.Value.js_consoleLog =<<
	enc <- encryptData aesKey "私は私とはぐれるわけには(いく/いかない)?"
	JS.Value.consoleLog enc
	ct <- JS.Object.js_get (JS.Value.toJSVal enc) (toJSString "cipherText")
	iv' <- JS.Object.js_get (JS.Value.toJSVal enc) (toJSString "iv")
	dc <- decryptData aesKey ct $ Uint8Array iv'
	JS.Value.js_consoleLog dc

generateKeyFromPassword :: String -> Uint8Array -> IO CryptoKey
generateKeyFromPassword password salt = do
	encoder <- newTextEncoder
	JS.Value.consoleLog encoder
	p <- encode encoder password
	JS.Value.consoleLog p
	p' <- js_importKey p
	js_deriveKey salt p'

newtype CryptoKey = CryptoKey JSVal

instance JS.Value.IsJSVal CryptoKey where toJSVal (CryptoKey ck) = ck
instance JS.Value.V CryptoKey

foreign import javascript interruptible
	"((p, cont) => { globalThis.crypto.subtle.importKey('raw', p, { name: 'PBKDF2' }, false, ['deriveKey']).then ((k) => cont(k)) })"
	js_importKey :: Uint8Array -> IO CryptoKey

foreign import javascript interruptible
	"((salt, pk, cont) => { globalThis.crypto.subtle.deriveKey({ name: 'PBKDF2', salt: salt, iterations: 100000, hash: 'SHA-256' }, pk, { name: 'AES-GCM', length: 256 }, true, ['encrypt', 'decrypt']).then((k) => cont(k)) })"
	js_deriveKey :: Uint8Array -> CryptoKey -> IO CryptoKey

encryptData :: CryptoKey -> String -> IO JS.Object.O
encryptData ck pt = do
	encoder <- newTextEncoder
	dataBuffer <- encode encoder pt
	JS.Value.consoleLog dataBuffer
	iv <- js_getRandomValues 12
	JS.Value.consoleLog iv
	cipherText <- encrypt iv ck dataBuffer
	o <- JS.Object.new
	JS.Object.set o "cipherText" cipherText
	JS.Object.set o "iv" iv
	JS.Object.freeze o

foreign import javascript "((n) => { return globalThis.crypto.getRandomValues(new Uint8Array(n)) })"
	js_getRandomValues :: Int -> IO Uint8Array

encrypt :: Uint8Array -> CryptoKey -> Uint8Array -> IO Uint8Array
encrypt iv k dtb = js_encrypt iv k dtb

foreign import javascript interruptible
	"((iv, k, dtb, cont) => { globalThis.crypto.subtle.encrypt({ name: 'AES-GCM', iv: iv }, k, dtb).then((e) => cont(e)) })"
	js_encrypt :: Uint8Array -> CryptoKey -> Uint8Array -> IO Uint8Array

decryptData :: CryptoKey -> JSVal -> Uint8Array -> IO JSVal
decryptData ck ct iv = do
	db <- js_decrypt iv ck ct
	decoder <- newTextDecoder
	decode decoder (JS.Value.toJSVal db)

foreign import javascript interruptible
	"((iv, k, ct, cont) => { globalThis.crypto.subtle.decrypt({ name: 'AES-GCM', iv: iv }, k, ct).then((d) => cont(d)) })"
	js_decrypt :: Uint8Array -> CryptoKey -> JSVal -> IO Uint8Array

foreign import javascript "(() => { return new TextDecoder() })"
	newTextDecoder :: IO JSVal

foreign import javascript "((d, b) => { return d.decode(b) })"
	decode :: JSVal -> JSVal -> IO JSVal
