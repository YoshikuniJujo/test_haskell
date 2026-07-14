{-# LANGUAGE JavaScriptFFI #-}

module Main where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value

main :: IO ()
main = demoECDSA

demoECDSA :: IO ()
demoECDSA = do
	JS.Value.js_consoleLog =<< js_keyPair

foreign import javascript interruptible "((cont) => { globalThis.crypto.subtle.generateKey( { name: 'ECDSA', namedCurve: 'P-256' }, true, ['sign', 'verify'] ).then((p) => cont(p)) })"
	js_keyPair :: IO JSVal
