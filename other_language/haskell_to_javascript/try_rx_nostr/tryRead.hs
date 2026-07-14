{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main where

import GHC.JS.Prim (JSVal)
import GHC.JS.Value qualified as JS.Value

main :: IO ()
main = do
	putStrLn "foobarbaz"
	JS.Value.consoleLog verifier
	JS.Value.consoleLog =<< createRxNostr verifier

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
