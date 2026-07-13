{-# LANGUAGE JavaScriptFFI #-}

module Main where

import GHC.JS.Prim (JSVal)

main :: IO ()
main = do
	putStrLn "hogepiyo"
	importAll
	putStrLn "foobar"
--	consoleLog m

foreign import javascript "(() => { const { createRxNostr } = await import('https://esm.sh') })"
	importAll :: IO ()

foreign import javascript "((x) => { console.log(x) })" consoleLog :: JSVal -> IO ()
