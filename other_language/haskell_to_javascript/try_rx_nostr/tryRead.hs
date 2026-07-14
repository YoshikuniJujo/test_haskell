{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value
import GHC.JS.Foreign.Callback

import Control.Concurrent

main :: IO ()
main = do
	putStrLn "foobarbaz"
	JS.Value.consoleLog verifier
	rn <- createRxNostr verifier
	setDefaultRelays rn ["wss://yabu.me"]
	JS.Value.consoleLog rn
	rfr <- createRxForwardReq
	JS.Value.consoleLog rfr
	o <- use rn rfr
	JS.Value.consoleLog o
	subscribe o JS.Value.consoleLog
	emit rfr 1
	threadDelay $ 10 * 1000000

newtype Verifier = Verifier JSVal
verifier = Verifier js_verifier

instance JS.Value.IsJSVal Verifier where toJSVal (Verifier v) = v
instance JS.Value.V Verifier

foreign import javascript "(() => { return verifier; })"
	js_verifier :: JSVal

newtype RxNostr = RxNostr JSVal
instance JS.Value.IsJSVal RxNostr where toJSVal (RxNostr n) = n
instance JS.Value.V RxNostr

createRxNostr :: Verifier -> IO RxNostr
createRxNostr (Verifier v) = RxNostr <$> js_createRxNostr v

foreign import javascript "((v) => { return createRxNostr({ verifier: v }) })"
	js_createRxNostr :: JSVal -> IO JSVal

use :: RxNostr -> RxForwardReq -> IO NostrObservable
use (JS.Value.toJSVal -> n) (JS.Value.toJSVal -> fr) = NostrObservable <$> js_use n fr

newtype NostrObservable = NostrObservable JSVal
instance JS.Value.IsJSVal NostrObservable where toJSVal (NostrObservable o) = o
instance JS.Value.V NostrObservable

foreign import javascript "((n, fr) => { return n.use(fr) })"
	js_use :: JSVal -> JSVal -> IO JSVal

setDefaultRelays :: RxNostr -> [String] -> IO ()
setDefaultRelays (JS.Value.toJSVal -> n) (JS.Value.toJSVal -> rs) = js_setDefaultRelays n rs

foreign import javascript "((n, rs) => { n.setDefaultRelays(rs) })"
	js_setDefaultRelays :: JSVal -> JSVal -> IO ()

newtype RxForwardReq = RxForwardReq JSVal
instance JS.Value.IsJSVal RxForwardReq where toJSVal (RxForwardReq f) = f
instance JS.Value.V RxForwardReq

createRxForwardReq :: IO RxForwardReq
createRxForwardReq = RxForwardReq <$> js_createRxForwardReq

foreign import javascript "(() => { return createRxForwardReq() })"
	js_createRxForwardReq :: IO JSVal

subscribe :: NostrObservable -> (Packet -> IO ()) -> IO ()
subscribe (JS.Value.toJSVal -> o) f = do
	let	f' = f . Packet
	f2 <- syncCallback1 ThrowWouldBlock f'
	js_subscribe o f2
	releaseCallback f2

newtype Packet = Packet JSVal
instance JS.Value.IsJSVal Packet where toJSVal (Packet p) = p
instance JS.Value.V Packet

foreign import javascript "((o, f) => { o.subscribe(f) })"
	js_subscribe :: JSVal -> Callback (JSVal -> IO ()) -> IO ()

emit :: RxForwardReq -> Int -> IO ()
emit (JS.Value.toJSVal -> n) k = js_emit n k

foreign import javascript "((n, k) => { n.emit({ kinds: [k] }) })"
	js_emit :: JSVal -> Int -> IO ()
