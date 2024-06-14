{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent

import GHC.JS.Prim
import GHC.JS.Foreign.Callback
import Data.IORef

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

foreign import javascript "(() => { console.log('foobar'); })"
	foobar :: IO ()

foreign import javascript "(() => { return foobarbaz; })" foobarbaz :: Int

foreign import javascript "getButton" getButton :: IO JSVal

foreign import javascript "setButtonListener"
	setButtonListener :: JSVal -> Callback (IO ()) -> IO ()

foreign import javascript "setButtonLabel"
	setButtonLabel :: JSVal -> JSVal -> IO ()

main :: IO ()
main = do
--	putFooBar <- syncCallback1 ThrowWouldBlock . const $ putStrLn "FooBar"
	putFooBar <- asyncCallback1 . const $ putStrLn "FooBar"
	winAddEventListenerLoad' putFooBar
--	releaseCallback putFooBar
	foo
	print 123
	foobar
	foobar
	print 123
	foobar
	print foobarbaz
--	setButtonListener
--	putFooBar <- syncCallback ThrowWouldBlock $ putStrLn "FooBarNew"
	b <- getButton
	c <- newIORef 0
	setButtonLabel b $ toJSString "foo"
	putFooBar <- syncCallback ThrowWouldBlock do
		setButtonLabel b . toJSString . ("hick " <>) . show =<< readIORef c
		modifyIORef c succ
	setButtonListener b putFooBar
	threadDelay 6000000
	print "finish"
