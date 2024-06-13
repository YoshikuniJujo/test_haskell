{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.JS.Prim
import GHC.JS.Foreign.Callback

foreign import javascript "((f) => { window.addEventListener('load', f()); })"
-- foreign import javascript "((f) => { window.addEventListener('load', console.log('hoge')); })"
-- foreign import javascript "((f) => { f(); })"
	winAddEventListenerLoad :: Callback (IO ()) -> IO ()

foreign import javascript "((f) => { window.addEventListener('load', (e) => { console.log('boo'); }); })"
-- foreign import javascript "((f) => { window.addEventListener('load', (e) => { f(e); }); })"
-- foreign import javascript "((f) => { window.addEventListener('load', console.log('hoge')); })"
-- foreign import javascript "((f) => { f(); })"
	winAddEventListenerLoad' :: Callback (JSVal -> IO ()) -> IO ()

foreign import javascript "(() => { console.log(window); window.addEventListener('load', (e) => { console.log('bee'); }); })"
-- foreign import javascript "(() => { window.onload = (e) => { console.log('bee'); } })"
	foo :: IO ()

main :: IO ()
main = do
--	putFooBar <- syncCallback1 ThrowWouldBlock . const $ putStrLn "FooBar"
	putFooBar <- asyncCallback1 . const $ putStrLn "FooBar"
	winAddEventListenerLoad' putFooBar
--	releaseCallback putFooBar
	foo
	print 123
