{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.JS.Prim

foreign import javascript "((id) => { return document.getElementById(id); })"
	js_getElementById :: JSVal -> IO (JSVal)

foreign import javascript "((e, t) => { e.textContent = t; })"
	js_setTextContent :: JSVal -> JSVal -> IO ()

foreign import javascript "((c) => { return c.getContext('2d'); })"
	js_getContext2d :: JSVal -> IO (JSVal)

foreign import javascript "((c) => { c.fillStyle = 'rgb(200, 0, 0)'; c.fillRect(10, 10, 50, 50); })"
	js_tryRectangle :: JSVal -> IO ()

foreign import javascript "((c) => { c.fillStyle = 'rgba(0, 0, 200, 0.5)'; c.fillRect(30, 30, 50, 50); })"
	js_tryRectangle2 :: JSVal -> IO ()

main :: IO ()
main = do
	foo <- js_getElementById (toJSString "foo")
	js_setTextContent foo (toJSString "bar")
	canvas <- js_getElementById (toJSString "canvas")
	ctx <- js_getContext2d canvas
	js_tryRectangle ctx
	js_tryRectangle2 ctx
	putStrLn "tryCanvas"
