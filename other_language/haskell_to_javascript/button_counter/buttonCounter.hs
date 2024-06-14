{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import GHC.JS.Prim
import GHC.JS.Foreign.Callback
import Data.IORef

foreign import javascript "((id) => { return document.getElementById(id); })"
	js_getElementById :: JSVal -> IO (JSVal)

foreign import javascript "((e, t) => { e.textContent = t; })"
	js_setTextContent :: JSVal -> JSVal -> IO ()

foreign import javascript "((t, et, f) => { t.addEventListener(et, f); })"
	js_addEventListenerSimple :: JSVal -> JSVal -> Callback (JSVal -> IO ()) -> IO ()

foreign import javascript "((e) => { e.preventDefault(); })"
	js_preventDefault :: JSVal -> IO ()

newtype Button = Button JSVal

getButtonById :: String -> IO Button
getButtonById idn = Button <$> js_getElementById (toJSString idn)

setButtonLabel :: Button -> String -> IO ()
setButtonLabel (Button btn) = js_setTextContent btn . toJSString

newtype Event = Event JSVal

addButtonClickListener :: Button -> (Event -> IO ()) -> IO ()
addButtonClickListener (Button btn) f = do
	f' <- syncCallback1 ThrowWouldBlock $ f . Event
	js_addEventListenerSimple btn (toJSString "click") f'

addButtonTouchstartListener :: Button -> (Event -> IO ()) -> IO ()
addButtonTouchstartListener (Button btn) f = do
	f' <- syncCallback1 ThrowWouldBlock $ f . Event
	js_addEventListenerSimple btn (toJSString "touchstart") f'

preventDefault :: Event -> IO ()
preventDefault (Event e) = js_preventDefault e

main :: IO ()
main = do
	putStrLn "begin"
	cnt <- newIORef (0 :: Int)
	b <- getButtonById "button0"
	addButtonClickListener b $ const do
		modifyIORef cnt (+ 1)
		c <- readIORef cnt
		setButtonLabel b $ "You push me " <> show c <> " time(s)!"
	addButtonTouchstartListener b \e -> do
		preventDefault e
		modifyIORef cnt (+ 1)
		c <- readIORef cnt
		setButtonLabel b $ "You push me " <> show c <> " time(s)!"
