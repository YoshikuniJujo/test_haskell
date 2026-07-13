module Main where

import GHC.JS.Prim (JSVal, toJSString)

main :: IO ()
main = do
	putStrLn "Hello"
	fooBar "from Haskell"

fooBar :: String -> IO ()
fooBar = js_FooBar . toJSString

foreign import javascript "((foo) => { GlobalLog(foo) })"
	js_FooBar :: JSVal -> IO ()
